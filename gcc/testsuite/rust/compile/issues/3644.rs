#![feature(no_core)]
#![no_core]

fn main() {
    extern "Rust" {
     fn main();
    }
    unsafe { main(); }
}