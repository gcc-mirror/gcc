// { dg-excess-errors "invalid crate name: ...." }
// { dg-bogus "unrecognized command-line option ...." }
#![feature(no_core)]
#![no_core]

fn main() {}
