// { dg-options "-w" }

#![feature(no_core, lang_items, unboxed_closures)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

pub enum Option<T> {
    None,
    Some(T),
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

pub trait Iterator2 {
    type Item;
    fn next(&mut self) -> Option<Self::Item>;
}

pub trait SplitIter: Iterator2 {
    fn finish(&mut self) -> Option<Self::Item>;
}

pub struct Split<'a, T, P>
where
    P: FnMut(&T) -> bool,
{
    v: &'a [T],
    pred: P,
}

impl<'a, T, P> Iterator2 for Split<'a, T, P>
where
    P: FnMut(&T) -> bool,
{
    type Item = &'a [T];
    fn next(&mut self) -> Option<&'a [T]> {
        Option::None
    }
}

impl<'a, T, P> SplitIter for Split<'a, T, P>
where
    P: FnMut(&T) -> bool,
{
    fn finish(&mut self) -> Option<&'a [T]> {
        Option::None
    }
}

struct GenericSplitN<I> {
    iter: I,
    count: usize,
}

impl<T, I: SplitIter<Item = T>> Iterator2 for GenericSplitN<I> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        match self.count {
            0 => Option::None,
            _ => {
                self.count -= 1;
                self.iter.next()
            }
        }
    }
}

pub struct SplitN<'a, T, P>
where
    P: FnMut(&T) -> bool,
{
    inner: GenericSplitN<Split<'a, T, P>>,
}

impl<'a, T, P> Iterator2 for SplitN<'a, T, P>
where
    P: FnMut(&T) -> bool,
{
    type Item = &'a [T];

    fn next(&mut self) -> Option<&'a [T]> {
        self.inner.next()
    }
}

fn main() -> i32 {
    0
}
