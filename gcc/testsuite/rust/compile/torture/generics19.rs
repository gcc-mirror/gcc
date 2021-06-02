struct Foo<X, Y>(X, Y);

impl<T> Foo<u32, T> {
    fn new(a: T) -> Self {
        Self(123, a)
    }
}

fn main() {
    let a;
    a = Foo::new(false);
}
