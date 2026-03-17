#![feature(no_core)]
#![no_core]

pub mod foo {
    pub macro bar() {}
}

use foo::bar;

fn main() {
    bar!();
}
