#![feature(no_core)]
#![no_core]

#[repr(i32)]
enum NightsWatch {} // { dg-error "unsupported representation for zero-variant enum" }
