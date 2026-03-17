#![feature(no_core)]
#![no_core]

macro_rules! foo {
    ($a:ident) => {}
}

pub fn bar() {
    foo!(self);
}
