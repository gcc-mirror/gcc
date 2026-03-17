#![feature(no_core)]
#![no_core]

#[doc]
// { dg-error "attribute must be of the form ...doc.hidden.inline....... or ...doc = string.." "" { target *-*-* } .-1 }
pub fn a(){}
