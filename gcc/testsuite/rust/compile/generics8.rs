#[lang = "sized"]
pub trait Sized {}

struct Foo<A, B>(A, B);

impl<T> Foo<i32, T> {
    fn test(a: T) -> T {
        a
    }
}

impl Foo<i32, f32> {
    fn test() -> f32 { // { dg-error "duplicate definitions with name .test." }
        123f32
    }
}

fn main() {}
