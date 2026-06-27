// { dg-additional-options "-frust-unused-check-2.0" }
#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

#[lang = "neg"]
pub trait Neg {
    type Output;
    fn neg(self) -> Self::Output;
}

pub fn f(x: i32) -> i32 {
    - -x
// { dg-warning "double negation" "" { target *-*-* } .-1 }
}
