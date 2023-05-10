// #407
pub fn loopy()  {
    let mut a = 1;
    loop {
        // { dg-error {failed to parse statement or expression in block expression} "" { target *-*-* } .-1 }
        if a < 40 {
            // { dg-error {failed to parse statement or expression in block expression} "" { target *-*-* } .-1 }
            a + = 1; // { dg-error "found unexpected token '=' in null denotation" }
            // { dg-error {failed to parse statement or expression in block expression} "" { target *-*-* } .-1 }
            // { dg-error {failed to parse if body block expression in if expression} "" { target *-*-* } .-2 }
            // { dg-error {could not parse loop body in \(infinite\) loop expression} "" { target *-*-* } .-3 }
            // { dg-error {unrecognised token 'integer literal' for start of item} "" { target *-*-* } .-4 }
            // { dg-error {failed to parse item in crate} "" { target *-*-* } .-5 }
        } else {
            break;
        }
    }
}
