extern "foobar" {
    // { dg-error "unknown ABI option" "" { target *-*-* } .-1 }
    fn printf(s: *const i8, ...);
}

pub extern "baz" fn test() {}
// { dg-error "unknown ABI option" "" { target *-*-* } .-1 }
