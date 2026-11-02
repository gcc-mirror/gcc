#![feature(no_core)]
#![no_core]

fn main() {
    let y = 3;
    enum Foo {
        Bar = y,
        // { dg-error "cannot find value .y. in this scope" "" { target *-*-* } .-1 }
    }
}