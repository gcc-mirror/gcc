// { dg-additional-options "-frust-unused-check-2.0" }
#![feature(no_core)]
#![no_core]

static TEST: usize = 1;
// { dg-warning "unused variable .TEST." "" { target *-*-* } .-1 }
