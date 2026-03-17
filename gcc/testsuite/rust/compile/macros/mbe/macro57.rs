#![feature(no_core)]
#![no_core]

macro_rules! macro_rules {
    () => {};
}

macro_rules! foo {
    () => {};
}

foo!();

fn main() {}

macro_rules!();
