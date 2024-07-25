// { dg-additional-options "-frust-crate-type=proc-macro" }

struct DummyStruct;

impl DummyStruct {
    pub fn method(self) {
        #[proc_macro]
        pub fn non_root_function() {} // { dg-error "functions tagged with .#.proc_macro.. must currently reside in the root of the crate" }
    }
}
