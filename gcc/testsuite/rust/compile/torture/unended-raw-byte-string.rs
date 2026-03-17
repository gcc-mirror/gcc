// { dg-excess-errors "...." }
#![feature(no_core)]
#![no_core]

fn main() {
    // { dg-error "unended raw byte string literal" "" { target *-*-* } .+1 }
    let s = br##"123"#
}

