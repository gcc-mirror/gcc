#![feature(no_core)]
#![no_core]

pub fn main() {
    let _: &i32 = &&&&1;
}
