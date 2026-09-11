#![feature(no_core)]
#![no_core]

#![feature(lang_items)]
#[lang = "sized"]
pub trait Sized {}

struct Foo<T>(T);

fn main() {
    &Foo(123);
}
