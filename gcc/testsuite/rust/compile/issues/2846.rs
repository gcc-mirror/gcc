#![feature(no_core)]
#![no_core]

struct S();

pub fn main() {
    match S() {
        S() => {}
    }
}
