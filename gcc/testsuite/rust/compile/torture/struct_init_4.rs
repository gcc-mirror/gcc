struct Foo {
    a: i32,
    // { dg-warning "field is never read" "" { target *-*-* } .-1 }
    b: i32,
    // { dg-warning "field is never read" "" { target *-*-* } .-1 }
}

fn main() {
    let a = Foo { a: 1, b: 2 };
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let b = Foo { a: 3, b: 4, ..a };
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
