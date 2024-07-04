extern "C" {
    pub fn dog(b: i32, a: ..., c: i32);
    // { dg-error "..... must be the last argument of a C-variadic function" "" { target *-*-* } .-1 }
    pub fn cat(a: ...);
    // { dg-error "C-variadic function must be declared with at least one named argument" "" { target *-*-* } .-1 }
}
