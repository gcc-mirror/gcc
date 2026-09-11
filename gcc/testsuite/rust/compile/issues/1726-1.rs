#![feature(lang_items, no_core)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

pub trait Foo {
    type Bar;
}

pub trait Copy {}

pub fn c<F: Foo<Bar: Foo>>()
where
    F::Bar: Copy,
{
}
