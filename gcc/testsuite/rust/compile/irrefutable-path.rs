#![feature(no_core)]
#![no_core]
enum E {
    A,
}

// TODO: this test ICEs because Path patterns in function parameters aren't fully implemented.
// once implementation is added this test should compile normally
fn a(E::A: E) {} // { dg-ice "" }
fn main() {
    let E::A: E;
}
