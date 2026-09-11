#![feature(no_core)]
#![no_core]

#[allow(unused_unsafe)]

pub fn foo() {
    unsafe { (0, 1) }.0;
}
