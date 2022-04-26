// { dg-additional-options "-w" }
struct Foo<T>(T);
impl<T> Foo<T> {
    fn new<Y>(a: T, b: Y) -> Self {
        Self(a)
    }
}

struct Bar<T>(T);
impl Bar<i32> {
    fn baz(self) {}

    fn test() -> i32 {
        123
    }
}

struct Baz<A, B>(A, B);
impl Baz<i32, f32> {
    fn test<X>(a: X) -> X {
        a
    }
}

pub fn main() {
    let a = Foo::<i32>::new::<f32>(123, 456f32);
    let b = Foo::new::<f32>(123, 456f32);

    let c = Bar::<i32>(123);
    let d = Bar::baz(c);

    let e = Bar::test();

    let f = Baz::test::<bool>(true);
}
