#![feature(no_core)]
#![no_core]

pub mod foo {
    pub fn bar() {}

    #[macro_export]
    macro_rules! qux {
        () => {};
    }
}

// is valid if this is uncommented
// use foo::*;

fn main() {
    bar(); // { dg-error "Cannot find path .bar. in this scope" }
}
