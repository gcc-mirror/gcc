#![feature(no_core)]
#![no_core]

#![feature(lang_items)]
#[lang = "sized"]
pub trait Sized {}

pub fn generic_function<X>(a: X) -> X {
    a
}
