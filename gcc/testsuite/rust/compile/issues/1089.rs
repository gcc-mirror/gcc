// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]

pub mod test_mod;

fn main() {
    let a = test_mod::Test(123);
}
