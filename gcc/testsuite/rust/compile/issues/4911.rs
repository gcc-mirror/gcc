// { dg-options "-w" }
#![feature(lang_items, no_core)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

pub trait Iterator2 {
    type Item;
}

pub trait IntoIterator2 {
    type IntoIter;
    type Item;
}

pub struct Flatten<I: Iterator2<Item: IntoIterator2>> {
    inner: I,
}

impl<I, U> Iterator2 for Flatten<I>
where
    I: Iterator2<Item: IntoIterator2<IntoIter = U, Item = U>>,
    U: Iterator2,
{
    type Item = U;
}

fn main() {}
