// { dg-options "-w" }
#![feature(no_core)]
#![no_core]

union B {
    a: A,
    b: f32,
}

struct A {
    data: i32,
    len: usize,
}
