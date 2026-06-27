// { dg-additional-options "-frust-unused-check-2.0" }
#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

#[no_mangle]
pub fn f<T>(_x: T) {}
// { dg-warning "generic functions must be mangled" "" { target *-*-* } .-1 }
