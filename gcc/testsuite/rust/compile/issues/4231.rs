#![feature(no_core)]
#![no_core]

#[repr = ""] // { dg-error "malformed .repr. attribute" }
struct ThreeInts {
    first: i16,
    second: i8,
    third: i32
}
