// { dg-additional-options "-frust-unused-check-2.0" }
#![feature(no_core)]
#![no_core]

pub fn a() ->i32 {
    let mut x = 2;
// { dg-warning "unused mut .x." "" { target *-*-* } .-1 }
    return x
}
