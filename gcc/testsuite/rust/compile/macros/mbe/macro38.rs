macro_rules! invalid_after_zeroable_multi {
    ($e:expr $(,)? $(;)* $(=>)? forbidden) => {{}}; // { dg-error "token .identifier. is not allowed after .expr. fragment" }
                                                    // { dg-error "required first macro rule" "" { target *-*-* } .-1 }
}
