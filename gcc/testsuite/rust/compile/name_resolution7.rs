// check that macros by example do not get inserted in ribs like regular items
#![feature(no_core)]
#![no_core]

pub mod foo {
    pub mod bar {
        pub mod baz {
            pub mod qux {
                macro_rules! foo {
                    (one) => {};
                }
            }
        }
    }
}

crate::foo::bar::baz::qux::foo!(); // { dg-error "could not resolve macro invocation" }
foo::bar::baz::qux::foo!(); // { dg-error "could not resolve macro invocation" }
