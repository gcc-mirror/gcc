#![feature(no_core)]
#![no_core]
fn _a(((_x, _y), _z): ((i32, i32), i32)) {}
fn _b((_x, (_y, _z)): (i32, (i32, i32))) {}
fn _c(&&_x: &&i32) {}
fn main() {
    let ((_a, _b), _c) = ((1, 2), 3);
    let (_a, (_b, _c)) = (1, (2, 3));
    let &&_x: &&i32;
}
