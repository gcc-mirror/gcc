macro_rules! inside_matcher {
    (($e:expr tok) tok) => {{}}; // { dg-error "token .identifier. is not allowed after .expr. fragment" }
                                 // { dg-error "failed to parse macro matcher" "" { target *-*-* } .-1 }
                                 // { dg-error "failed to parse macro match" "" { target *-*-* } .-2 }
                                 // { dg-error "required first macro rule" "" { target *-*-* } .-3 }
                                 // { dg-error "failed to parse item in crate" "" { target *-*-* } .-4 }
}
