// This already worked before the #409 code changes.
fn test()  {
    let mut a = 1;
    a + = 1; // { dg-error "found unexpected token '=' in null denotation" }
    // { dg-error {failed to parse statement or expression in block expression} "" { target *-*-* } .-1 }
    // { dg-error {unrecognised token 'integer literal' for start of item} "" { target *-*-* } .-2 }
    // { dg-error {failed to parse item in crate} "" { target *-*-* } .-3 }
}
