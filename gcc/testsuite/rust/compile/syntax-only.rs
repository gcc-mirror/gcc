// { dg-additional-options "-fsyntax-only" }
#![feature(no_core)]
#![no_core]


fn main() {
    let mut a = 15;
    a = true;
}
