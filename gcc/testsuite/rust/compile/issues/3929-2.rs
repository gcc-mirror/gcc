// { dg-options "-w" }
#![feature(no_core)]
#![no_core]

struct S {
    x: i32,
    y: i32,
}

fn main() {
    let s = S{x: 1, y: 2};
    match s {
        S{x: 1, ..} => {}
    }
}
