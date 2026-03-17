#![feature(no_core)]
#![no_core]

pub struct TupStruct (i32, i32);

pub fn main() {
    let t = TupStruct (1, 2);
    match t {
        TupStruct { 0: 1, 1: 2 } => {}
        TupStruct { 1: b, .. } => {}
        TupStruct { 0: 1, .. } => {}
        _ => {}
    }
}
