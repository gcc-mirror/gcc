// { dg-additional-options "-frust-unused-check-2.0" }
#![feature(no_core)]
#![no_core]

pub fn foo() {
    /// useless
    let _x = 5;
// { dg-warning "unused doc comment" "" { target *-*-* } .-1 }
    let _y = _x;
}
