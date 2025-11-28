// Braced macro invocations are not allowed as match arms without a semicolon,
// even though they are allowed as statements without a semicolon.

macro_rules! m {
    () => { 1 }
}

fn h(c: bool) {
    match c {
        true => m! {}
        false => ()
        // { dg-error "exprwithoutblock requires comma after match case expression in match arm \\(if not final case\\)" "" { target *-*-* } .-1 }
    };
}

fn main () {}
