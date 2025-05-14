#[lang = "sized"]
pub trait Sized {}

trait Foo<T> {
    type A;

    fn test(a: Self::A) -> Self::A {
        a
    }
}

struct Bar<T>(T);
impl<T> Foo<T> for Bar<i32> {
    type A = T;
}

fn main() {
    let a;
    a = Bar(123);

    let b;
    b = Bar::test(a.0);
}
