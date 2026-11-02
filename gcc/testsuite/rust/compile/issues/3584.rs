#![feature(no_core)]
#![no_core]

enum E {
    A = {
        enum E { B }
        0
    }
}

fn main() {}