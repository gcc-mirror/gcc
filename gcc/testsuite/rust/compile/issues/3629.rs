#![feature(no_core)]
#![no_core]

fn main() {
    let mut p = Point { x: 3, y: 4 };
    // { dg-error "could not resolve path .Point. .E0433." "" { target *-*-* } .-1 }
    let q = &p;

    p.x = 5;
    q.x;
}