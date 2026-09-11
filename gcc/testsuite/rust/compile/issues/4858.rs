#![feature(no_core)]
#![no_core]

pub fn signed(x: isize) -> isize {
    !x
}

pub fn unsigned(x: usize) -> usize {
    !x
}
