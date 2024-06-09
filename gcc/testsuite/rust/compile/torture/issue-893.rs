// { dg-additional-options "-w" }
#[lang = "sized"]
pub trait Sized {}

struct Foo<T>(T);
impl<T> Foo<T> {
    fn new<Y>(a: T, b: Y) -> Self {
        Self(a)
    }
}

pub fn test() {
    let a = Foo::<i32>::new::<f32>(123, 456f32);
}
