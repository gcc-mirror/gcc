#![feature(no_core)]
#![no_core]

#[repr(C)]
enum NightsWatch {} // { dg-error "unsupported representation for zero-variant enum" }
