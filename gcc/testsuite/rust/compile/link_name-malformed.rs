// { dg-options "-w" }
#![feature(no_core)]
#![no_core]

#[link_name] // { dg-error "malformed .link_name. attribute input" }
fn foo() {}

// { dg-note "must be of the form" "" { target *-*-* } .-3 }
