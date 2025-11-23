// { dg-additional-options "-w" }
#![feature(lang_items)]
#[lang = "sized"]
pub trait Sized {}

impl<T> *const T {
    fn test(self) {}
}
