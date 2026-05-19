#![feature(no_core)]
#![no_core]
fn a(&1: &i32) {} // { dg-error "refutable pattern" }
fn main() {
    let &1: &i32; // { dg-error "refutable pattern" }
}
