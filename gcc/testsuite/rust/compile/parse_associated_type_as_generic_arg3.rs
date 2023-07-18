#[lang = "sized"]
pub trait Sized {}

trait Bar {
    type B;

    fn bar();
}

trait Foo {
    type A;

    fn foo();
}

trait Toto {
    type C;

    fn toto();
}

trait Tata {
    type D;
    fn tata();
}

impl Toto for u32 {
    type C = f32;

    fn toto() {}
}

impl Tata for f32 {
    type D = u32;

    fn tata() {}
}

struct S; // { dg-warning "struct is never constructed" }

impl Bar for i32 {
    type B = u32;

    fn bar() {}
}

impl Foo for S {
    type A = i32;

    fn foo() {}
}

enum Maybe<T> {
    Something(T),
    Nothing,
}

pub fn foo() -> Maybe<<<<<S as Foo>::A as Bar>::B as Toto>::C as Tata>::D> {
    Maybe::Something(15)
}
