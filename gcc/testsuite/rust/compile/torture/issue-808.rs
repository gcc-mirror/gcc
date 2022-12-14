pub trait Foo {
    type Target;

    fn bar(&self) -> &Self::Target;
}

impl<T> Foo for &T {
    type Target = T;

    fn bar(&self) -> &T {
        *self
    }
}

pub fn main() {
    let a: i32 = 123;
    let b: &i32 = &a;

    b.bar();
}
