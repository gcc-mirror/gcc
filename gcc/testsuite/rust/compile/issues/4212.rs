#![feature(no_core)]
#![no_core]

#![derive(PartialOrd, PartialEq)]
// { dg-error "attribute cannot be used at crate level" "" { target *-*-* } .-1 }
// { dg-error "could not resolve trait .PartialOrd." "" { target *-*-* } .-2 }
// { dg-error "could not resolve trait .PartialEq." "" { target *-*-* } .-3 }
pub fn check_ge(a: i32, b: i32) -> bool {
    a >= b
}
