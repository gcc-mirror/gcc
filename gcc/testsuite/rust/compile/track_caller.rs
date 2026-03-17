#![feature(no_core)]
#![no_core]

#[track_caller]
fn foo() {}

fn main() {
    foo();
}
