#![feature(no_core)]
#![no_core]
fn _a(_: i32) {}
fn _b(_x: i32) {}
fn _d((_x, _y): (i32, i32)) {}
fn _e((_x, .., _y): (i32, i32, i32, i32)) {}
fn _f((..): (i32, i32)) {}
fn _g((_x, (..)): (i32, (i32, i32))) {}
fn main() {
    let _ = 1;
    let _x = 1;
    let (_a, _b) = (1, 2);
    let (_a, .., _b) = (1, 2, 3, 4);
    let (..) = (1, 2);
    let (_a, (..)) = (1, (2, 3));
}
