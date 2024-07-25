// { dg-additional-options "-frust-crate-type=proc-macro" }

fn outer_function() {
    #[proc_macro_attribute]
    pub fn non_root_function() {} // { dg-error "functions tagged with .#.proc_macro_attribute.. must currently reside in the root of the crate" }
}
