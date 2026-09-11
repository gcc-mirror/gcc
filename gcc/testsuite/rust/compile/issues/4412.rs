// { dg-options "-frust-incomplete-and-experimental-compiler-do-not-use" }
#![feature(no_core)]
#![no_core]
struct Bug([u8; panic!"\t"]);
// { dg-error "unexpected token" "" { target *-*-* } .-1 }
// { dg-error "failed to parse" "" { target *-*-* } .-2 }
// { dg-error "could not parse" "" { target *-*-* } .-3 }
// { dg-error "expecting" "" { target *-*-* } .-4 }
fn main() {}