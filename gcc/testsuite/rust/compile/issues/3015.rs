#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
trait Sized {}

impl<T, const N: usize> [T; N] {}
