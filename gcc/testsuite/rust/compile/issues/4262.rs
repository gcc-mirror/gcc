#![feature(no_core)]
#![no_core]

#[cfg]
// { dg-error ".cfg. is not followed by parentheses" "" { target *-*-* } .-1 }
fn a() {}
