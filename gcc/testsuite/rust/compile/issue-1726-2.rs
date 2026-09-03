#![feature(lang_items, no_core)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

pub trait Foo {
    type Bar;

    fn foo(&self);
}

pub trait Copy {}

pub fn c<F: Foo<Bar: Foo>>(value: &F::Bar)
where
    F::Bar: Copy,
{
    value.foo();
}

pub struct Outer;
pub struct Inner;

impl Foo for Outer {
    type Bar = Inner;

    fn foo(&self) {}
}

impl Foo for Inner {
    type Bar = Inner;

    fn foo(&self) {}
}

impl Copy for Inner {}

pub fn main() {
    let inner = Inner;
    c::<Outer>(&inner);
}
