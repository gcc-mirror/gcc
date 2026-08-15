#![feature(no_core)]
#![no_core]
#![feature(lang_items)]

#[lang = "sized"]
trait Sized {}

pub struct S<const N: u32 = u32::MAX>;
// { dg-error "expressions must be enclosed in braces to be used as const generic arguments" "" { target *-*-* } .-1 }
