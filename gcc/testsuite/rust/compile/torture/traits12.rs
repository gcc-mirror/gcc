trait Foo {
    type A;

    fn test(a: Self::A) -> Self::A {
        a
    }
}

struct Bar(i32);
// { dg-warning "struct is never constructed" "" { target *-*-* } .-1 }

impl Foo for Bar {
    type A = i32;
}

struct Baz(f32);
// { dg-warning "struct is never constructed" "" { target *-*-* } .-1 }

impl Foo for Baz {
    type A = f32;
}

fn main() {
    let a: <Baz as Foo>::A;
    a = 123f32;

    let b;
    b = <Baz as Foo>::test(a);
}
