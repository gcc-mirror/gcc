struct Foo {
    a: i32,
    b: f32,
}

fn main() {
    let a = Foo { a: 1, b: 2f32 };
    let b = Foo { b: 4f32, ..a };
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
