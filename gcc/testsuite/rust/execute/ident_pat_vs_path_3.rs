// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]

enum E {
    A,
    B,
    C
}

fn main() -> i32 {
    use E::C;

    let v1 = match (E::A,) {
        (C,) => 1,
        (E::A,) => 0,
        (E::B,) => 1
    };

    let v2 = match (E::A,) {
        (B,) => 0,
        (E::A,) => 1,
        (C,) => 1
    };

    v1 + v2
}
