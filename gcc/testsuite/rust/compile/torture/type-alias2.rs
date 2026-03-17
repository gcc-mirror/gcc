#![feature(no_core)]
#![no_core]

type x = u32;

fn main() {
    let x: x = 1;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let y: x = 2;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
