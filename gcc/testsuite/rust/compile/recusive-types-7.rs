// { dg-additional-options "-w" }

#![feature(no_core)]
#![no_core]

struct Bad {
    bad: Bad, // { dg-error "recursive type .Bad. has infinite size" }
}

fn main() {}
