trait Foo<T> {
    type A;

    fn test(a: T) -> T {
        a
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

    let c: <Bar<i32> as Foo<i32>>::A;
    c = <Bar<i32> as Foo<i32>>::test(a.0);
}
