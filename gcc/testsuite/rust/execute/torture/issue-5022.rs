#![feature(no_core, lang_items)]
#![no_core]

// Rust-GCC/gccrs#5022: reading through a projection to a generic impl type

#[lang = "sized"]
pub trait Sized {}

pub trait Pattern {
    type Searcher;
}

pub struct Pat<C>(C);

pub struct Srch<C> {
    c: (C,),
}

impl<C> Pattern for Pat<C> {
    type Searcher = Srch<C>;
}

fn get(s: &<Pat<i32> as Pattern>::Searcher) -> i32 {
    s.c.0
}

fn main() -> i32 {
    let s = Srch { c: (7,) };
    get(&s) - 7
}
