// { dg-additional-options "-frust-unused-check-2.0" }
#![feature(no_core)]
#![no_core]

pub fn foo() {
    'a: loop {
        break 'a loop {};
// { dg-warning "easy to confuse" "" { target *-*-* } .-1 }
    }
}
