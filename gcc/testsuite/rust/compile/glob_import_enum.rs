#![feature(no_core)]
#![no_core]

use self::Ordering::*;
use Ordering::*;

enum Ordering {
    A,
    B,
}

fn foo(_: Ordering) {}

fn main() {
    let a = A;

    foo(a);
    foo(B);
}
