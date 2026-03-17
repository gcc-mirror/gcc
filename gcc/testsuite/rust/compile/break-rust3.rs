#![feature(no_core)]
#![no_core]

fn main() {
    break rust;
    // { dg-ice "are you trying to break rust? how dare you?" }
}
