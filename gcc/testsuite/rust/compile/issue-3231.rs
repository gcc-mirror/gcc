// { dg-options "-w" }
#![feature(no_core)]
#![no_core]

pub enum X {}

pub fn foo(x: X) {
    let _a: i32 = match x {};
}

pub fn main() {}
