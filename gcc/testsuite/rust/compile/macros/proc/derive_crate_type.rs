// { dg-additional-options "-frust-crate-type=lib" }

trait Dungeness {}

#[proc_macro_derive(Dungeness)] // { dg-error "the .#.proc_macro_derive.. attribute is only usable with crates of the .proc-macro. crate type" }
pub fn my_invalid_macro() {}
