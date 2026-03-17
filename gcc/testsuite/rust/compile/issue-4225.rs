#![feature(no_core)]
#![no_core]
// { dg-options "-w" }

#[allow] // { dg-error "malformed .allow. attribute input" }
fn foo() {}
// { dg-note "must be of the form" "" { target *-*-* } .-2 }

#[allow(dead_code)] // This is a correctly formed attribute
fn bar() {}
