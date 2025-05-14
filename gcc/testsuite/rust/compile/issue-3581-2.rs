enum A {
    X { inner: i32 },
    Y,
}

pub fn test() {
    let _ = A::Y.inner;
    // { dg-error "no field .inner. on type .A. .E0609." "" { target *-*-* } .-1 }
}
