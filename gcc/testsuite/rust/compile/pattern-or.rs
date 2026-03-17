// { dg-additional-options "-fsyntax-only" }
#![feature(no_core)]
#![no_core]


fn main() {
    match ((12, 13)) {
        (_, 5) | (12, _) => {}
    }
}
