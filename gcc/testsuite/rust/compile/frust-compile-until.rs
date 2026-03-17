// { dg-additional-options "-frust-compile-until=unsafety" }
#![feature(no_core)]
#![no_core]


unsafe fn foo() {}

fn main() {
    foo()
}
