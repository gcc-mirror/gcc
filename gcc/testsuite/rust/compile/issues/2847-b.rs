#![feature(no_core)]
#![no_core]

pub fn test() -> i32 {
    let (a, _) = (1, 2);
    a
}
