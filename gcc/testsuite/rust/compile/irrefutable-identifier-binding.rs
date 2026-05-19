#![feature(no_core)]
#![no_core]
fn _a(_x @ _: i32) {}
fn _b(_x @ (_a, _b): (i32, i32)) {}
fn main() {
    let _x @ _ = (1, 2);
    let _x @ (_a, _b) = (1, 2);
}
