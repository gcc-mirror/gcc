// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]

enum E {
    A,
}

struct S;

fn main() {
    // A single-variant enum's unit variant is irrefutable.
    let E::A: E;
    // A unit struct is irrefutable.
    let S: S;
}
