// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]

fn test(a: i32, _: i32) {
    let _ = 42 + a;
}
