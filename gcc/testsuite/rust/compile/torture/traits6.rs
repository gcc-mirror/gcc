#[lang = "sized"]
pub trait Sized {}

trait Foo {
    type A;

    fn baz(a: Self::A) -> Self::A;
}

struct Bar<T>(T);

impl<T> Foo for Bar<T> {
    type A = T;

    fn baz(a: Self::A) -> T {
        a
    }
}

fn main() {
    let a;
    a = Bar::<i32>::baz(123);
}
