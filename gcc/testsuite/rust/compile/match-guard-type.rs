#![feature(no_core)]
#![no_core]

fn main() -> i32 {
    match 0 {
        0 if 5 => 10, // { dg-error "mismatched types" }
        _ => 20,
    }
}
