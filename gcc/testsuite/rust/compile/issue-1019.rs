#[lang = "sized"]
pub trait Sized {}

trait A<T> {
    type Output;

    fn test(self, a: &T) -> &Self::Output;
}

struct Foo<T> {
    // { dg-warning "struct is never constructed" "" { target *-*-* } .-1 }
    start: T,
    end: T,
}

impl<X> A<X> for Foo<usize> {
    type Output = X;

    fn test(self, a: &X) -> &Self::Output {
        a
    }
}
