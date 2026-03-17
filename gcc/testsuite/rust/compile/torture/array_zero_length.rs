#![feature(no_core)]
#![no_core]

fn main() {
    let arr = ["Hello"; 0];
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
