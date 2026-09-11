#![feature(no_core)]
#![no_core]

fn main() {
    match (0, 92, 29) {
        (pat, ..) => {}
    }
}