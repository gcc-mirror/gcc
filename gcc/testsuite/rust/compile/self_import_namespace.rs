// { dg-additional-options "-frust-name-resolution-2.0" }

mod bar {
    pub mod foo {}
    pub fn foo() {}
}

// This only imports the module `foo`. The function `foo` lives in
// the value namespace and is not imported.
use bar::foo::{self};

fn main() {
    foo(); // { dg-error "expected value" }
}
