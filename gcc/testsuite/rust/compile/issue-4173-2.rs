#![feature(no_core)]
#![no_core]
#![feature(lang_items)]

#[lang = "sized"]
trait Sized {}

pub struct S<const N: u32 = { u32::MAX }>;
// { dg-error "failed to resolve path segment using an impl Probe" "" { target *-*-* } .-1 }
