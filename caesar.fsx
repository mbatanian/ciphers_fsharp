// Basic implementation of a Caesar cipher

let shiftWord w s =
    let alphaLength = 26
    
    let shiftImpl (c: char) bnd shft =
        let n = ((int c) - bnd + shft) % alphaLength + bnd
        (char n)
    
    let shiftChar s c =
        match System.Char.IsLetter(c) with
        |   true when System.Char.IsUpper(c) -> shiftImpl c (int 'A') s
        |   true when System.Char.IsLower(c) -> shiftImpl c (int 'a') s
        |   _ -> c
    
    let sc = shiftChar s
    String.map sc w

match fsi.CommandLineArgs with
    | [| scriptName; text; shift; |] ->
        let sint = System.Int32.Parse(shift)
        printfn "%s" (shiftWord text sint)
    | _ ->
        printfn "USAGE: [text] [shift]"
