#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

pub trait A<T> {}

pub struct B<T: A<T>> (T);
