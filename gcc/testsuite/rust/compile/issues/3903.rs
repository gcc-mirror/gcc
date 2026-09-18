// { dg-options "-w" }

#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

trait Foo {
    type Value;
}
impl Foo for u32 {
    type Value = f64;
}
struct A {
    T: <u32 as Foo>::Value,
}
