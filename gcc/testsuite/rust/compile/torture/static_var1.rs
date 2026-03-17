#![feature(no_core)]
#![no_core]

static x:i32 = 3;

fn main() {
    let y = x +1;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
