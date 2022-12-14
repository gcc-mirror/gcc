trait Foo {
    type A;

    fn test(a: Self::A) -> Self::A {
        a
    }
}

struct Bar(i32);
impl Foo for Bar {
    type A = i32;
}

struct Baz(f32);
impl Foo for Baz {
    type A = f32;
}

fn main() {
    let a;
    a = Bar(123);

    let b;
    b = Bar::test(a.0);

    let c;
    c = Baz(123f32);

    let d;
    d = Baz::test(c.0);
}
