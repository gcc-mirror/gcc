#![feature(no_core)]
#![no_core]

#![derive(PartialOrd, PartialEq)]
// { dg-error "attribute cannot be used at crate level" "" { target *-*-* } .-1 }
pub fn check_ge(a: i32, b: i32) -> bool {
    a >= b
}
