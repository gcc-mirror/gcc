#![feature(no_core)]
#![no_core]

fn main() {
    let arr = [];
    // { dg-error "type annotations needed" "" { target *-*-* } .-1 }
}
