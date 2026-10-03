#![feature(no_core)]
#![no_core]

fn main() {}

pub <<><>< MAX_CONNECTIONS: u32 = 100; // { dg-error "expected item, found" }
