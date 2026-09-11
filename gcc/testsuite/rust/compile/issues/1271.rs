// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]

fn test() {
    let a: &str = "TEST 1";
    let b: &str = &"TEST 2";
}
