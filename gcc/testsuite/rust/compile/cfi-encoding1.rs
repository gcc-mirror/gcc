#![feature(no_core)]
#![no_core]

#[cfi_encoding = "4Bleh"] // { dg-error "experimental feature" }
pub struct Bleh(i32);
