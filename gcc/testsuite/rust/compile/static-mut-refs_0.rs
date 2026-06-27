// { dg-additional-options "-frust-unused-check-2.0" }
#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

static mut S: i32 = 0;

pub unsafe fn f() {
    let _y = &S;
// { dg-warning "reference to a mutable static" "" { target *-*-* } .-1 }
}
