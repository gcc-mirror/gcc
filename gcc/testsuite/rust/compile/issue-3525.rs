// { dg-options "-w" }
#![feature(no_core)]
#![no_core]


struct Foo(usize);

const B: usize = A.0;
const A: Foo = Foo(123);
