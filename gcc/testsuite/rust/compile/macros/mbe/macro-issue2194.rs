#![feature(no_core)]
#![no_core]

macro_rules! foo {($type:ident) => {
    let $type = 12;
}} 

pub fn foo() {
    foo!(_a);
}
