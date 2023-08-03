extern "foobar" {
    // { dg-error "invalid ABI: found .foobar." "" { target *-*-* } .-1 }
    fn printf(s: *const i8, ...);
}

pub extern "baz" fn test() {}
// { dg-error "invalid ABI: found .baz." "" { target *-*-* } .-1 }

// extern "Rust" fn foo() {} // OK!