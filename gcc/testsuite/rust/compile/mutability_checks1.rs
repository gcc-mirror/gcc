pub fn test() {
    let a;
    a = 1;
    a = 2 + 1;
    // { dg-error "assignment of read-only variable" "" { target *-*-* } .-1 }

    struct Foo(i32);
    let a = Foo(1);
    a.0 = 2;
    // { dg-error "assignment of read-only variable" "" { target *-*-* } .-1 }

    let a = [1, 2, 3, 4];
    a[0] = 1 + 2;
    // { dg-error "assignment of read-only variable" "" { target *-*-* } .-1 }
}
