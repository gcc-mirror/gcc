#![feature(no_core)]
#![no_core]

mod foo {
    pub fn foo() {}
}

use foo::foo;

fn main() {
    foo();
}
