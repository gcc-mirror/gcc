#![feature(lang_items, no_core)]
#![no_core]

#[lang = "sized"]
trait Sized {}

trait IntoIterator {
    type IntoIter;
}

pub struct Chain<A, B>(A, B);

trait Iterator {
    fn chain<U>(self, other: U) -> Chain<Self, U::IntoIter>
    where
        Self: Sized,
        U: IntoIterator;
}
