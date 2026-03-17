#![feature(no_core)]
#![no_core]

macro_rules! foo {
    ($(+)+) => {e};
    () => {}
}

foo!();
