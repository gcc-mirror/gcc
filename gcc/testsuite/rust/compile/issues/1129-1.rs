// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]

fn write_u8(i: u8) {
    let x: &[u8] = &[i];
}
