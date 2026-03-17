// { dg-excess-errors "...." }
#![feature(no_core)]
#![no_core]

fn main() {
    // { dg-error "unended byte string literal" "" { target *-*-* } .+1 }
    let s = b"123
}
