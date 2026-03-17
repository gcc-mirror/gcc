#![feature(no_core)]
#![no_core]

unsafe mod toto {}
// { dg-error "module cannot be declared unsafe" "" { target *-*-* } .-1 }
