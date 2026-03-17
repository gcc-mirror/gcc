// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]

fn main() {
    let (a, .., b) = (2, 3);
}
