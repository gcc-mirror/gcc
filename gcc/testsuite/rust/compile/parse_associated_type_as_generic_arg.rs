#[lang = "sized"]
pub trait Sized {}

trait Foo {
    type A;

    fn foo();
}

struct S; // { dg-warning "struct is never constructed" }

impl Foo for S {
    type A = i32;

    fn foo() {}
}

enum Maybe<T> {
    Something(T),
    Nothing,
}

pub fn foo() -> Maybe<<S as Foo>::A> {
    Maybe::Something(15)
}
