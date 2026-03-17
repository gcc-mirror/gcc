// { dg-additional-options "-frust-unused-check-2.0" }
#![feature(no_core)]
#![no_core]


pub fn MyFunction() {
// { dg-warning "function .MyFunction. should have a snake case name" "" { target *-*-* } .-1 }
    let _MyVar = 2;
// { dg-warning "variable ._MyVar. should have a snake case name" "" { target *-*-* } .-1 }
}

pub fn foo<'MyLifetime>(x: &'MyLifetime i32) -> &'MyLifetime i32 {
// { dg-warning "lifetime .MyLifetime. should have a snake case name" "" { target *-*-* } .-1 }
    x
}

mod MyModule {}
// { dg-warning "module .MyModule. should have a snake case name" "" { target *-*-* } .-1 }

