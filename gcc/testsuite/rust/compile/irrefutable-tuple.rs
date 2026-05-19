#![feature(no_core)]
#![no_core]
enum E {
    A(i32),
}
fn _f((E::A(x), y): (E, i32)) {}
fn main() {
    let (E::A(x), y): (E, i32);
}
