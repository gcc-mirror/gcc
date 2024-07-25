// { dg-additional-options "-frust-crate-type=proc-macro" }

trait SomeTrait {}

struct DummyStruct;

impl DummyStruct {
    pub fn method(self) {
        #[proc_macro_derive(SomeTrait)]
        pub fn non_root_function() {} // { dg-error "functions tagged with .#.proc_macro_derive.. must currently reside in the root of the crate" }
    }
}
