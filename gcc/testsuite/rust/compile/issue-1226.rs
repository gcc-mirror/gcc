// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]

const TEST: *mut u8 = 123 as *mut u8;

fn test() {
    let a = TEST;
}
