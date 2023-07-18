// { dg-additional-options "-frust-crate-type=lib" }

#[proc_macro_attribute] // { dg-excess-errors "the '#\[proc_macro_attribute\]' attribute is only usable with crates of the 'proc-macro' crate type" }
pub fn my_invalid_macro() {}
