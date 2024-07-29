macro_rules! invalid_after_zeroable {
    ($e:expr $(,)* forbidden) => {{}}; // { dg-error "token .identifier. is not allowed after .expr. fragment" }
                                       // { dg-error "required first macro rule" "" { target *-*-* } .-1 }
                                       // { dg-error "failed to parse item in crate" "" { target *-*-* } .-2 }
}
