#![feature(no_core)]
#![feature(cfi_encoding)]
#![no_core]

#[cfi_encoding = "4Bleh"] // { dg-warning "does nothing" }
pub struct Bleh(i32);
