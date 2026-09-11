// { dg-options "-fsyntax-only" }
#![feature(no_core)]
#![no_core]

trait A {
    fn a(&self) -> <Self as A>::X;
}

impl A for u32 {}

fn main() {
    let a: u32 = 0;
    let b: u32 = a.a();
}
