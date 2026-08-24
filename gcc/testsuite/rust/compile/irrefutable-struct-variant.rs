#![feature(no_core)]
#![no_core]
enum E {
    S { x: i32 },
}
enum T {
    T(i32, i32),
}
fn _a(E::S { x }: E) {}
fn _b(E::S { x: _y }: E) {}
fn _c(T::T { 0: _y, 1: _z }: T) {}
fn main() {
    let E::S { x }: E;
    let E::S { x: _y }: E;
    let T::T { 0: _y, 1: _z }: T;
}
