#![feature(no_core)]
#![no_core]

pub(in crate::) struct S;
// { dg-error "expecting ... but .::. found" "" { target *-*-* } .-1 }
