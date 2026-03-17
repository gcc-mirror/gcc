// { dg-additional-options "-frust-debug" }
#![feature(no_core)]
#![no_core]


// Just scan for one of the Rust front end debug diagnostics:
// { dg-message {note: Attempting to parse file: .+/gcc/testsuite/rust/compile/debug-diagnostics-on\.rs} "" { target *-*-* } 0 }

fn main() {
}
