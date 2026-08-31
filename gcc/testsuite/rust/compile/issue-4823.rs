#![feature(no_core)]
#![no_core]

pub struct S;

trait T {
    const VALUE: i32;
}

impl T for S {
    const VALUE: i32 = 1;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}

impl S {
    const VALUE: i32 = 2;
}

const RESULT: i32 = S::VALUE;

pub fn test() -> i32 {
    RESULT
}
