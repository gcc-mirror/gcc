#![feature(no_core)]
#![no_core]

macro_rules! foo {
    () => { #[cfg(all())] 12 }
}

fn main() -> i32 {
    foo!()
}
