#![feature(no_core)]
#![no_core]

#![cold] // { dg-error "attribute cannot be used at crate level" }
pub fn test() {}
