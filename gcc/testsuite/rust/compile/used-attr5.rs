#![feature(no_core)]
#![no_core]

#[used] // { dg-error "may only be applied to static items" }
pub fn foo() {}
