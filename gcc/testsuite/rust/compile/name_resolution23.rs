// { dg-options "-frust-name-resolution-2.0" }

mod a {
    pub mod b {
        pub fn foo() {}
        pub fn bar() {}
        pub fn baz() {}
    }
    pub fn baz() {}
}

use a::b::*;
use a::baz;

pub fn func() {
    baz();
    foo();
    bar();
}
