// { dg-options "-w" }
#![feature(no_core)]
#![no_core]

struct S();

fn main() {
    let s = S{};
    match s {
        S{..} => {}
    }
}
