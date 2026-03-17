// { dg-additional-options "-fno-rust-debug" }
#![feature(no_core)]
#![no_core]


// Make sure we don't see any 'note's:
// { dg-bogus {note: } "" { target *-*-* } 0 }

fn main() {
}
