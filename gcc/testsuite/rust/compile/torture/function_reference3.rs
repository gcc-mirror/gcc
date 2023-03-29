struct Foo {
    a: fn(i32) -> i32,
    b: i32,
}

fn test(a: i32) -> i32 {
    a + 1
}

fn main() {
    let a = test(1);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let b: fn(i32) -> i32 = test;
    let c = b(1);

    let d = Foo { a: test, b: c };
    let e = (d.a)(d.b);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
