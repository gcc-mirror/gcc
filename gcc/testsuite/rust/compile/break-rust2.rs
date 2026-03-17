#![feature(no_core)]
#![no_core]

fn main() {
    break (rust);
    // { dg-error "cannot find value .rust. in this scope" "" { target *-*-* } .-1 }
}
