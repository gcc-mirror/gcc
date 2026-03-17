#![feature(no_core)]
#![no_core]

fn f() {}

macro_rules! panic {
    () => (
        crate::f()
    );
}

fn main() {
    panic!();
}
