// { dg-additional-options "-frust-crate-type=proc-macro" }
#![feature(no_core)]
#![no_core]


#[proc_macro_attribute]
fn my_macro() {} // { dg-error "functions tagged with .#.proc_macro_attribute.. must be .pub." }
