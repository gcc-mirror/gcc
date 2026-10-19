// { dg-options "-w" }
#![feature(no_core, lang_items, unboxed_closures)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

pub trait Iterator2 {
    type Item;
}

#[lang = "fn_once"]
#[rustc_paren_sugar]
pub trait FnOnce<Args> {
    #[lang = "fn_once_output"]
    type Output;

    extern "rust-call" fn call_once(self, args: Args) -> Self::Output;
}

#[lang = "fn_mut"]
#[rustc_paren_sugar]
pub trait FnMut<Args>: FnOnce<Args> {
    extern "rust-call" fn call_mut(&mut self, args: Args) -> Self::Output;
}

pub struct FilterMap<I, F> {
    iter: I,
    f: F,
}

impl<B, I: Iterator2, F> Iterator2 for FilterMap<I, F>
where
    F: FnMut(I::Item) -> Option<B>,
{
    type Item = B;
}

pub enum Option<T> {
    None,
    Some(T),
}

fn main() -> i32 {
    0
}
