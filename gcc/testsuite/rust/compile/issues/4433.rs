// { dg-options "-frust-incomplete-and-experimental-compiler-do-not-use" }
#![feature(no_core)]
#![no_core]

#[derive(Toto)] // { dg-error "macro not found" }
// { dg-error "could not resolve trait" "" { target *-*-* } .-1 }
struct Test {}

#[derive(
    Happy,   // { dg-error "macro not found" }
    // { dg-error "could not resolve trait" "" { target *-*-* } .-1 }
    Birthday // { dg-error "macro not found" }
    // { dg-error "could not resolve trait" "" { target *-*-* } .-1 }
)]
struct MultiLine {}

#[derive(One, Two)] // { dg-error "macro not found" }
// { dg-error "could not resolve trait" "" { target *-*-* } .-1 }
struct SingleLine {}