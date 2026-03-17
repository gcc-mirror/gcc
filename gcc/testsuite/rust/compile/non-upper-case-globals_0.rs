// { dg-additional-options "-frust-unused-check-2.0" }
#![feature(no_core)]
#![no_core]


static _my_static : i32 = 0;
// { dg-warning "static variable ._my_static. should have an upper case name" "" { target *-*-* } .-1 }
