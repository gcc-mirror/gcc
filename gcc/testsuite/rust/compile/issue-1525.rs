#![feature(no_core)]
#![no_core]

fn main() {
    const slice: &[i32] = &[1, 2, 3];
    let _slice2: &[i32] = slice;
}
