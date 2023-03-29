// #407
pub fn loopy()  {
    let mut a = 1;
    // { dg-error {failed to parse expr with block in parsing expr statement} "" { target *-*-* } .+2 }
    // { dg-error {failed to parse statement or expression without block in block expression} "" { target *-*-* } .+1 }
    loop {
        // { dg-error {failed to parse expr with block in parsing expr statement} "" { target *-*-* } .+2 }
        // { dg-error {failed to parse statement or expression without block in block expression} "" { target *-*-* } .+1 }
	if a < 40 {
	    a + = 1; // { dg-error "found unexpected token '=' in null denotation" }
            // { dg-error {failed to parse expression for expression without block \(pratt-parsed expression is null\)} "" { target *-*-* } .-1 }
            // { dg-error {failed to parse statement or expression without block in block expression} "" { target *-*-* } .-2 }
            // { dg-error {failed to parse if body block expression in if expression} "" { target *-*-* } .-3 }
            // { dg-error {could not parse loop body in \(infinite\) loop expression} "" { target *-*-* } .+1 }
	} else {
	    break;
	}
    }
}
// { dg-error {unrecognised token '\}' for start of item} "" { target *-*-* } .-1 }
// { dg-error {failed to parse item in crate} "" { target *-*-* } .-2 }
