#![feature(no_core)]
#![no_core]

fn main() {
    let x = 1;

    match x {
        2 => {},
        a @ 3 => {},
        _ => {},
    }
}
