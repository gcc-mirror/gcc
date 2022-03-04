trait Foo {
    type A;
    type B;

    fn new(a: Self::A, b: Self::B) -> Self;
}

struct Baz(i32, f32);

impl Foo for Baz {
    type A = i32;
    type B = f32;

    fn new(a: Self::A, b: Self::B) -> Self {
        Baz(a, b)
    }
}

fn main() {
    Baz::new(123, 456f32);
}
