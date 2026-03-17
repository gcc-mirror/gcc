#![feature(no_core)]
#![no_core]

pub struct TupStruct (i32, i32);

pub fn main() -> i32 {
    let mut t = TupStruct (1, 1);
    match t {
        TupStruct { 0: 1, 1: b } => { b -= 1 }
        _ => {}
    }
    b
}
