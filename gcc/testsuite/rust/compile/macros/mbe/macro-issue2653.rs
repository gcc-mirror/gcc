#![feature(no_core)]
#![no_core]

macro_rules! foo {
    ($p:path $b:block) => {};
}

fn main() {}
