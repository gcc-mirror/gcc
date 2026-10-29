// { dg-additional-options "-w" }
#![feature(no_core, lang_items)]
#![no_core]

// Rust-GCC/gccrs#5022: same as issue-5022.rs, but the bound check in g
// resolved W's field before the Pattern impl had been checked, so the
// impl's params were never bound.

#[lang = "sized"]
pub trait Sized {}

pub mod clone {
    #[lang = "clone"]
    pub trait Clone: crate::Sized {
        fn clone(&self) -> Self;
    }
}

impl clone::Clone for u32 {
    fn clone(&self) -> Self {
        *self
    }
}

fn need<X: clone::Clone>(_x: X) {}

pub fn g(x: u32) {
    need(x);
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

pub struct W(<Pat<u8> as Pattern>::Searcher);

impl clone::Clone for W {
    fn clone(&self) -> Self {
        W(clone::Clone::clone(&self.0))
    }
}
