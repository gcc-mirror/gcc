#![feature(no_core)]
#![no_core]

fn main() {
    const array:[i32; 1] = [1];
    const slice:&[i32] = &array;
}
