#![feature(no_core, lang_items)]
#![no_core]
#[lang = "sized"]
pub trait Sized {}
trait Iterator {
    type Item;
}
pub struct Wrapper<I>(I);
impl<T, I: Iterator<Item = T>> Iterator for Wrapper<I> {
    type Item = T;
}
