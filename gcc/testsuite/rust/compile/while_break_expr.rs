#![feature(no_core)]
#![no_core]

fn main() {
    let _ = 'l: while break 'l {};
}
