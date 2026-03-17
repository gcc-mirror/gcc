// { dg-additional-options "-w -frust-cfg=A" }
#![feature(no_core)]
#![no_core]

#![cfg_attr(not(B), allow(dead_code))]
#![cfg_attr(A, allow(while_true))]

#[cfg_attr(not(B), allow(deprecated))]
#[cfg_attr(A, allow(drop_bounds))]
fn main() { }
