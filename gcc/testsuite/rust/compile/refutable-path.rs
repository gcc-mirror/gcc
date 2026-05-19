#![feature(no_core)]
#![no_core]
// { dg-ice "" }
enum E {
    A,
    B,
}
const A: u32 = 0;
fn a(E::A: E) {}
fn b(A: u32) {}
fn main() {
    let A: u32;
    let E::A: E;
}
