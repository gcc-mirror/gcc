// { dg-options "-fsyntax-only" }
#![feature(no_core)]
#![no_core]


fn main() {
    for a in 0..10 {}
    (for b in 0..10 {})
}
