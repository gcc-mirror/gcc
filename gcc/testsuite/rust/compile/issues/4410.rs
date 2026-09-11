// { dg-options "-frust-incomplete-and-experimental-compiler-do-not-use -w" }
#![feature(no_core)]
#![no_core]

// The bug was that a bare #[deprecated] (with no arguments) caused a Segfault.
// This test ensures it compiles without crashing.

#[deprecated]
pub mod a {}

fn main() {}