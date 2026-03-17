#![feature(no_core)]
#![no_core]

fn foo() {
    1 = 3; // { dg-error "assignment of read-only location" }
}

fn main() {
    foo();
}
