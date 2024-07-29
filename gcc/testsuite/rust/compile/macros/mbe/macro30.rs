macro_rules! m {
    ($e:expr $f:expr) => {
        // { dg-error "fragment is not allowed after .expr. fragment" "" { target *-*-* } .-1 }
        // { dg-error "required first macro rule in macro rules definition could not be parsed" "" { target *-*-* } .-2 }
        // { dg-error "failed to parse item in crate" "" { target *-*-* } .-3 }
        $e
    };
}
