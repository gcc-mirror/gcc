#![feature(no_core)]
#![no_core]

#![feature(lang_items)]

#[lang = "sized"]
trait Sized {}

pub enum ROption<T> {
    RSome(T),
    RNone,
}

fn main() {}
