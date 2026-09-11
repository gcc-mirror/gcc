#![feature(lang_items, no_core)]
#![no_core]

#[lang = "sized"]
trait Sized {}

trait Iterator {
    type Item;
}

trait IntoIterator {
    type IntoIter;
}

pub struct FlattenCompat<I, U>(I, U);

pub struct Flatten<I>
where
    I: Iterator,
    I::Item: IntoIterator,
{
    pub inner: FlattenCompat<I, <I::Item as IntoIterator>::IntoIter>,
}
