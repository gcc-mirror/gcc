#![feature(no_core)]
#![no_core]

type A = crate::A;
// { dg-error "failed to resolve type path segment: .A." "" { target *-*-* } .-1 }
