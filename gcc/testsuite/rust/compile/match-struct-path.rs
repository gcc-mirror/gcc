#![feature(no_core)]
#![no_core]

pub struct S;

pub fn foo(v: S) {
    match v {
        S => ()
    }
}
