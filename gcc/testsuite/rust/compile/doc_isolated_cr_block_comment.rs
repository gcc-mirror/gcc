#![feature(no_core)]
#![no_core]

// { dg-error "Isolated CR" "" { target *-*-* } .+1 }
/** doc cr comment */
pub fn main () { }
