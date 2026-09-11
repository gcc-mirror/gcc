// { dg-additional-options "-fsyntax-only" }
#![feature(no_core)]
#![no_core]


fn main() {
    let is_zero = &|&&d: &&u8| -> bool { d == b'0' };
    let lambda = |&c| c != b'9';
}
