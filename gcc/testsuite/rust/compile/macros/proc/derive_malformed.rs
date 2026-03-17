// { dg-additional-options "-frust-crate-type=proc-macro" }
#![feature(no_core)]
#![no_core]


#[proc_macro_derive] // { dg-excess-errors "malformed 'proc_macro_derive' attribute input" }
pub fn my_invalid_macro() {}
