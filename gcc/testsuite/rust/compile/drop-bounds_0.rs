// { dg-additional-options "-frust-unused-check-2.0" }
#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

#[lang = "drop"]
pub trait Drop {
    fn drop(&mut self);
}

pub fn f<T: Drop>(_x: T) {}
// { dg-warning "bounds on .Drop. are most likely incorrect" "" { target *-*-* } .-1 }
