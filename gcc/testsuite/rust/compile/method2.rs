struct Foo<A, B>(A, B);

impl Foo<i32, f32> {
    fn test<X>(self, a: X) -> X {
        a
    }
}

fn main() {
    let a;
    a = Foo(123, 456f32);

    let b;
    b = a.test::<asfasfr>(false);
    // { dg-error "could not resolve type path .asfasfr." "" { target *-*-* } .-1 }
}
