// { dg-do compile }
// { dg-options "-w" }

#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

pub unsafe trait Searcher<'a> {
    fn haystack(&self) -> &'a ();
}

pub trait Pattern<'a>: Sized {
    type Searcher: Searcher<'a>;
}

pub trait MultiCharEq {}

struct MultiCharEqPattern<C: MultiCharEq>(C);

struct MultiCharEqSearcher<'a, C: MultiCharEq> {
    marker: &'a (),
    c: C,
}

impl<'a, C: MultiCharEq> Pattern<'a> for MultiCharEqPattern<C> {
    type Searcher = MultiCharEqSearcher<'a, C>;
}

unsafe impl<'a, C: MultiCharEq> Searcher<'a> for MultiCharEqSearcher<'a, C> {
    fn haystack(&self) -> &'a () {
        self.marker
    }
}

impl<'x> MultiCharEq for &'x [i32] {}

pub struct CharSliceSearcher<'a, 'b>(<MultiCharEqPattern<&'b [i32]> as Pattern<'a>>::Searcher);

fn main() -> i32 {
    0
}
