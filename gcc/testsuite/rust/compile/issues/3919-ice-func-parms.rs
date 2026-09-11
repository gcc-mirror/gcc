#![feature(no_core)]
#![no_core]

fn func(1: i32) {} // { dg-error "refutable pattern in function argument" }
