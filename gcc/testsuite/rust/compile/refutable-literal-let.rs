#![feature(no_core)]
#![no_core]
fn main() {
    let 1: i32; // { dg-error "refutable pattern in local" }
}
