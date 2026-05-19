#![feature(no_core)]
#![no_core]
fn f(a @ 1: i32){} // { dg-error "refutable pattern" }
fn main() {
    let a @ 1: i32; // { dg-error "refutable pattern" }
}
