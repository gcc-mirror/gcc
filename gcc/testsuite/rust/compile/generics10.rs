struct Foo<A, B>(A, B);

impl<X = i32> Foo<X, f32> { // { dg-error "defaults for type parameters are only allowed in .struct., .enum., .type., or .trait. definitions" }
    fn new(a: X, b: f32) -> Self {
        Self(a, b)
    }
}

fn main() {
    let a;
    a = Foo::new(123, 456f32);
}
