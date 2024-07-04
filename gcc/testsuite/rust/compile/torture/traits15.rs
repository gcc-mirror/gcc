#[lang = "sized"]
pub trait Sized {}

trait Foo<T> {
    type A;

    fn test(a: T, b: Self::A) -> (T, Self::A) {
        (a, b)
    }
}

struct Bar<T>(T);
impl<T> Foo<T> for Bar<T> {
    type A = T;
}

pub fn main() {
    let a;
    a = Bar(123);

    let b: <Bar<i32> as Foo<i32>>::A;
    b = 456;

    let c;
    c = <Bar<i32> as Foo<i32>>::test(a.0, 123);
}
