#![feature(no_core)]
#![no_core]

#[cfg()]
// { dg-error "malformed .cfg. attribute input" "" { target *-*-* } .-1 }
fn a() {}
