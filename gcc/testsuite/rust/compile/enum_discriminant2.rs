#![feature(no_core)]
#![no_core]

fn test() -> isize {
    1
}

enum Foo {
    Bar = test() // { dg-error "only functions marked as .const." }
}

fn main() {}
