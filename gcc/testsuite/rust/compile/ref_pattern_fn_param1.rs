#![feature(no_core)]
#![no_core]

fn f(&b: i32) {} // { dg-error "expected i32, found reference" }
