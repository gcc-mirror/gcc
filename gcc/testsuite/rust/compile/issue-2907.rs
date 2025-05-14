#![feature(lang_items)]

#[lang = "sized"]
pub trait Sized {}

pub trait Bar {}

pub trait Foo {
    type Ty;

    fn foo(self) -> Self::Ty;
}

impl<B: Bar> Foo for B {
    type Ty = u32;

    fn foo(self) -> Self::Ty {
        // { dg-warning "unused name" "" { target *-*-* } .-1 }
        14
    }
}

struct Qux;

impl Bar for Qux {}

fn main() {
    let a = Qux;
    a.foo();

    let b = Qux;
    Foo::foo(b);
}
