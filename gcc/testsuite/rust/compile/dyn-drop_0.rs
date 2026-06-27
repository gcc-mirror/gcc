// { dg-additional-options "-frust-unused-check-2.0" }
#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

#[lang = "drop"]
pub trait Drop {
    fn drop(&mut self);
}

pub fn f(_x: &dyn Drop) {}
// { dg-warning "trait object has a .Drop. bound" "" { target *-*-* } .-1 }
