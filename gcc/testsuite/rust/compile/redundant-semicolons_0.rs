// { dg-additional-options "-frust-unused-check-2.0" }
#![feature(no_core)]
#![no_core]


pub fn foo() -> i32 
{
    let a = 32;;
// { dg-warning "unnecessary trailing semicolons" "" { target *-*-* } .-1 }
    return a
}


