struct Foo {
    a: i32,
// { dg-warning "field is never read" "" { target *-*-* } .-1 }
    b: i32,
// { dg-warning "field is never read" "" { target *-*-* } .-1 }
}

fn main() {
    let a = 1;
    let b = 2;
    let c = Foo { a, b };
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
