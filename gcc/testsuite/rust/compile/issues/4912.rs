#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

pub trait Searcher<'a> {}

pub trait ReverseSearcher<'a>: Searcher<'a> {}

fn main() {}
