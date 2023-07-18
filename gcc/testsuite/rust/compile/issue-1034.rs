#[lang = "sized"]
pub trait Sized {}

trait Foo<T> {
    type Output;

    fn test(self, slice: &T) -> &Self::Output;
}

struct Bar<T>(T);
// { dg-warning "struct is never constructed" "" { target *-*-* } .-1 }

impl<T> Foo<[T]> for Bar<usize> {
    type Output = [T];

    fn test(self, slice: &[T]) -> &[T] {
        slice
    }
}
