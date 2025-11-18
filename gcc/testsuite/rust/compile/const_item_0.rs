// { dg-additional-options "-frust-unused-check-2.0" }
#![feature(no_core)]
#![no_core]

const A: usize = 1;
// { dg-warning "deadcode const item .A." "" { target *-*-* } .-1 }
