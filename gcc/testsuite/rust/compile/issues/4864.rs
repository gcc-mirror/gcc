#![feature(no_core, lang_items)]
#![no_core]
#[lang = "sized"]
pub trait Sized {}
trait Iterator {
    type Item;
}
trait IntoIterator {
    type Item;
    type IntoIter: Iterator<Item = Self::Item>;
}
pub struct Flatten<I, U> {
    pub inner: I,
    pub extra: U,
}
impl<I, U> Iterator for Flatten<I, U>
where
    I: Iterator<Item: IntoIterator<IntoIter = U, Item = U::Item>>,
    U: Iterator,
{
    type Item = U::Item;
}
