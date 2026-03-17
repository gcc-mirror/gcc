// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]

macro_rules! foo {
    {} => {
        15
    };
}

fn main() {
    let a = foo!();
    let b = foo![];
}
