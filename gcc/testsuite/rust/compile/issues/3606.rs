// { dg-options "-w" }
#![feature(no_core)]
#![no_core]

#[repr()]
pub struct Coord {
    x: u32,
    y: u32,
}
