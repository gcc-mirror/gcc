#[lang = "sized"]
pub trait Sized {}

struct Foo<A, B>(A, B);

impl<T> Foo<T, T> {
    fn new(a: T, b: T) -> Self {
        Self(a, b)
    }
}

fn main() {
    let a;
    a = Foo::new(123, 456);
}
