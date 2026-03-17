#![feature(no_core)]
#![no_core]

#![feature(decl_macro)]
macro one() {
    1
}

fn main() -> i32 {
    one!() - 1
}
