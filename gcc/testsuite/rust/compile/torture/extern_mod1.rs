// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]

mod modules;

fn main() {
    let twelve = modules::return_12();
}
