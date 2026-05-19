#![feature(no_core)]
#![no_core]
// TODO: this test ICEs because slices in function parameters aren't fully implemented.
// once implementation is added this test should compile normally
// { dg-ice "" }
fn a([a, b, c]: [i32; 3]) {}
fn b([a, .., c]: [i32; 3]) {}
fn c([a, .., c]: [i32; 5]) {}
fn main() {
    let [..]: [i32];
    let [a, b, c]: [i32; 3];
    let [a, .., c]: [i32; 3];
    let [a, .., c]: [i32; 5];
}
