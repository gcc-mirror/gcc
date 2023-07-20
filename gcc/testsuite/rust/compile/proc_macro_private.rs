// { dg-additional-options "-frust-crate-type=proc-macro" }

#[proc_macro]
fn my_macro() {} // { dg-error "functions tagged with .#.proc_macro.. must be .pub." }
