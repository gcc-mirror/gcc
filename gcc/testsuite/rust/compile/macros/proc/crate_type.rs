// { dg-additional-options "-frust-crate-type=lib" }
#![feature(no_core)]
#![no_core]


#[proc_macro] // { dg-error "the .#.proc_macro.. attribute is only usable with crates of the .proc-macro. crate type" }
pub fn my_invalid_macro() {}
