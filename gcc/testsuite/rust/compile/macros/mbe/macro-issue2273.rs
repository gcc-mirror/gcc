#![feature(no_core)]
#![no_core]

macro_rules! mac {
    () => {();} // { dg-warning "trailing semicolon" }
}

pub fn foo() {
    mac!()
}
