// TODO: allow(unused) should be disabling "function is never used" warnings
// { dg-additional-options "-w" }
#![no_core]
#![feature(no_core)]

mod a {
    pub mod b {
        pub struct X;
    }
}

use a::*;

#[allow(unused)]
fn b() {}

trait Y {}

impl Y for b::X {}
