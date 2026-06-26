// { dg-additional-options "-frust-unused-check-2.0" }
#![feature(no_core)]
#![no_core]

pub fn foo() {
    let _x = { 5 };
// { dg-warning "unnecessary braces around assigned value" "" { target *-*-* } .-1 }
}
