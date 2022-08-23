struct Foo {
    a: i32,
    b: f32,
// { dg-warning "field is never read" "" { target *-*-* } .-1 }
}

fn main() {
    let c = Foo { a: 1, b: 2f32 };
    let b = Foo { b: 4f32, ..c };
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
