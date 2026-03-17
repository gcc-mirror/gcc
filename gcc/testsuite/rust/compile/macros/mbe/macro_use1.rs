#![feature(no_core)]
#![no_core]

#[macro_use]
mod foo {
    macro_rules! a {
        () => {};
    }

    macro_rules! b {
        () => {};
    }
}

fn main() {
    a!();
    b!();
}
