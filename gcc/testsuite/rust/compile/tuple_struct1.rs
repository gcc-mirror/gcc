struct Foo {
    one: i32,
    two: i32,
}

fn main() {
    let a = Foo(1, 2); // { dg-error "expected function, tuple struct or tuple variant, found struct 'Foo'" }
}
