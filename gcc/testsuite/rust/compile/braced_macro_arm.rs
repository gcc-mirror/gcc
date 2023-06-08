// Braced macro invocations are not allowed as match arms without a semicolon,
// even though they are allowed as statements without a semicolon.

macro_rules! m {
    () => { 1 }
}

fn h(c: bool) {
    match c {
        // { dg-error "failed to parse statement or expression in block expression" "" { target *-*-* } .-1 }
        true => m! {}
        false => ()
        // { dg-error "exprwithoutblock requires comma after match case expression in match arm \\(if not final case\\)" "" { target *-*-* } .-1 }
        // { dg-error "unrecognised token .false. for start of item" "" { target *-*-* } .-2 }
        // { dg-error "failed to parse item in crate" "" { target *-*-* } .-3 }
    };
}

fn main () {}
