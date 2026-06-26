// { dg-additional-options "-frust-unused-check-2.0" }
#![feature(no_core)]
#![no_core]

pub fn foo() {
    unsafe {}
// { dg-warning "unnecessary .unsafe. block" "" { target *-*-* } .-1 }
}
