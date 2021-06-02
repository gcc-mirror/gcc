struct Foo {
    a: i32,
    b: i32,
}

fn main() {
    let a = Foo { a: 1, b: 2 };
    let b = Foo { ..a };
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
