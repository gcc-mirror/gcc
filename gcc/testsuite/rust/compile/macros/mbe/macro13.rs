// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]

macro_rules! create_type {
    ($s:ident) => {
        struct $s;
    };
}

fn main() {
    create_type!(A);

    let a = A;
}
