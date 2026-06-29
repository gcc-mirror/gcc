// { dg-additional-options "-frust-compile-until=compilation -frust-compat-version=1.71" }
#![feature(no_core)]
#![no_core]


pub struct Foo {
    a: i32,
}

fn main() {
    let _ = offset_of!(Foo, a); // valid
}
