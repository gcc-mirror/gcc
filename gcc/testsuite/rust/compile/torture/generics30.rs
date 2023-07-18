#[lang = "sized"]
pub trait Sized {}

struct Foo<A, B>(A, B);

impl<T> Foo<T, f32> {
    fn test<X>(self, a: X) -> X {
        // { dg-warning "unused name" "" { target *-*-* } .-1 }
        a
    }
}

fn main() {
    let a;
    a = Foo(123, 456f32);

    let b;
    b = a.test::<bool>(false);
}
