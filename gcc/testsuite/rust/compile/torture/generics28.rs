struct Foo<A, B>(A, B);

impl Foo<i32, f32> {
    fn test<X>(a: X) -> X {
        a
    }
}

fn main() {
    let a;
    a = Foo::test::<_>(123);

    let b;
    b = Foo::test::<bool>(true);

    let c;
    c = Foo::test(456f32);
}
