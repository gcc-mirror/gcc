struct Foo<A, B>(A, B);

impl<X = i32> Foo<X, f32> { // { dg-error "defaults for type parameters are not allowed here" }
    fn new(a: X, b: f32) -> Self {
        Self(a, b)
    }
}

fn main() {
    let a;
    a = Foo::new(123, 456f32);
}
