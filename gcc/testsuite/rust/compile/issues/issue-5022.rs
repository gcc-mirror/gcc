// { dg-additional-options "-w" }
#![feature(no_core, lang_items)]
#![no_core]

// Rust-GCC/gccrs#5022: a projection to an impl type that uses the impl's
// params, like <Pat<u8> as Pattern>::Searcher with Searcher = Srch<C>,
// kept C instead of u8 and codegen ICEd on it.

#[lang = "sized"]
pub trait Sized {}

pub mod clone {
    #[lang = "clone"]
    pub trait Clone: crate::Sized {
        fn clone(&self) -> Self;
    }
}

pub trait Pattern {
    type Searcher;
}

pub struct Pat<C>(C);

pub struct Srch<C> {
    c: *const C,
}

impl<C> Pattern for Pat<C> {
    type Searcher = Srch<C>;
}

impl<C> clone::Clone for Srch<C> {
    fn clone(&self) -> Self {
        Srch { c: self.c }
    }
}

pub fn by_value(_s: <Pat<u8> as Pattern>::Searcher) {}

pub fn by_ref(s: &<Pat<u8> as Pattern>::Searcher) {
    let _c = clone::Clone::clone(s);
}

pub struct W(<Pat<u8> as Pattern>::Searcher);

impl clone::Clone for W {
    fn clone(&self) -> Self {
        W(clone::Clone::clone(&self.0))
    }
}
