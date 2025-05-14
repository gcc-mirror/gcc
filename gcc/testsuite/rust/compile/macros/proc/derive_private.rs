// { dg-additional-options "-frust-crate-type=proc-macro" }

trait Chesapeake {}

#[proc_macro_derive(Chesapeake)]
fn my_macro() {} // { dg-error "functions tagged with .#.proc_macro_derive.. must be .pub." }
