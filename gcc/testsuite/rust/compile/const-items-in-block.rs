#![feature(no_core)]
#![no_core]

fn main() -> i32 {
    const fn one() -> i32 {
        1
    }
    const X: i32 = 1;

    one() - X
}
