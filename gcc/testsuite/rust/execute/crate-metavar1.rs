#![feature(no_core)]
#![no_core]

macro_rules! foo {
    () => {
        $crate::bar()
    }
}

pub fn bar() -> i32 { 1 }

fn main() -> i32 {
    foo!() - crate::bar()
}
