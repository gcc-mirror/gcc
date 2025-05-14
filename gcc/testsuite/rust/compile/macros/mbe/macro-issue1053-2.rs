macro_rules! m {
    ($e:expr $(forbidden)*) => {{}}; // { dg-error "token .identifier. is not allowed after .expr. fragment" }
                                     // { dg-error "required first macro rule in macro rules definition could not be parsed" "" { target *-*-* } .-1 }
                                     // { dg-error "failed to parse item in crate" "" { target *-*-* } .-2 }
}
