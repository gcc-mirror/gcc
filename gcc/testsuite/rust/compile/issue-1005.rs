// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]

#![feature(lang_items)]
#[lang = "sized"]
pub trait Sized {}

impl<T> *const T {
    fn test(self) {}
}
