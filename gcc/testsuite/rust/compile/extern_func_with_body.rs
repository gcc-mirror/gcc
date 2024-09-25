extern "C" {
    fn myfun0(a:i32,...) {}
    // { dg-error "cannot have a body" "" { target *-*-* } .-1 }
}

