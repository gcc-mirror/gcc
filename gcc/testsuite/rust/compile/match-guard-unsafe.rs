#![feature(no_core)]
#![no_core]

unsafe fn check() -> bool {
    true
}

fn main() -> i32 {
    match 0 {
        0 if check() => 1, // { dg-error "call to unsafe function requires unsafe function or block" }
        _ => 0,
    }
}
