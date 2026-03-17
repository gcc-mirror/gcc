#![feature(no_core)]
#![no_core]

fn foo() {}

fn main() {
    let _a = &foo;
}