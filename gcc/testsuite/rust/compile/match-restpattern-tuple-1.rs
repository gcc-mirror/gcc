#![feature(no_core)]
#![no_core]

fn main() {
    let x = (1, 2, 3, 4);

    match x {
        (1, .., 4) => {},
        _ => {}
    }
}