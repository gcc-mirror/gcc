#[lang = "sized"]
pub trait Sized {}

pub trait MyBinaryTrait<Rhs = Self> {
    fn do_something(&self, rhs: &Rhs);
}

struct Foo<T> {
    // { dg-warning "struct is never constructed" "" { target *-*-* } .-1 }
    value: T,
}

impl<T> MyBinaryTrait for Foo<T> {
    fn do_something(&self, _rhs: &Self) {}
}
