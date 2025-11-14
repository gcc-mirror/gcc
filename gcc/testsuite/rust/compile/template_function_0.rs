// { dg-additional-options "-frust-unused-check-2.0" }
#![feature(lang_items)]
#[lang = "sized"]
pub trait Sized {}

pub fn test<T> (a: usize) -> () {
    // { dg-warning "unused variable .a." "" { target *-*-* } .-1 }
}
