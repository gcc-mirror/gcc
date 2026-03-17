#![feature(no_core)]
#![no_core]

fn func() -> i32 { // { dg-error "mismatched types, expected .i32. but got ...." }
}

fn main() {
    func();
}
