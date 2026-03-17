// { dg-excess-errors "...." }
#![feature(no_core)]
#![no_core]

fn main() {
    // { dg-error "unended string literal" "" { target *-*-* } .+1 }
    let s = "123
}
