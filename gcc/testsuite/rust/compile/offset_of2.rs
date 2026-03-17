// { dg-additional-options "-frust-compile-until=compilation -frust-assume-builtin-offset-of" }
#![feature(no_core)]
#![no_core]


pub struct Foo {
    a: i32,
}

fn main() {
    let _ = offset_of!(Foo, a); // valid
}
