// { dg-options "-w" }
#![feature(no_core)]
#![no_core]

#[no_mangle(foo)] // { dg-error "malformed .no_mangle. attribute input" }
fn foo() {}
// { dg-note "must be of the form" "" { target *-*-* } .-2 }
