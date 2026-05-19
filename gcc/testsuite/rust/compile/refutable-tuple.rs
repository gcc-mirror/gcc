#![feature(no_core)]
#![no_core]
fn a((x, 1): (i32, i32)) {} // { dg-error "refutable pattern" }
fn b((x, .., 1): (i32, i32, i32)) {} // { dg-error "refutable pattern" }
fn c((1, (..)): (i32, (i32, i32))) {} // { dg-error "refutable pattern" }
fn main() {
    let (a, 1) = (1, 2); // { dg-error "refutable pattern" }
    let (x, .., 1): (i32, i32, i32); // { dg-error "refutable pattern" }
    let (1, (..)): (i32, (i32, i32)); // { dg-error "refutable pattern" }
}
