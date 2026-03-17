#![feature(no_core)]
#![no_core]

const X: i32 = 1;

pub fn foo() -> [i32; 1] {
    [::X]
}
