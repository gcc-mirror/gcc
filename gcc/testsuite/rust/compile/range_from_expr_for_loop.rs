// { dg-additional-options "-frust-compile-until=ast" }
#![feature(no_core)]
#![no_core]

fn main() {
    for _ in 1.. {
        break;
    }
    let i = 2;
}
