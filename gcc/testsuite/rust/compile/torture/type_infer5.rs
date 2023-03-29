struct Foo {
    a: i32,
    b: i32,
// { dg-warning "field is never read" "" { target *-*-* } .-1 }
}

fn main() {
    let a;
    a = Foo { a: 1, b: 2 };

    let b = a.a;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
