#![feature(no_core)]
#![feature(intrinsics)]
#![no_core]

extern "rust-intrinsic" {
    fn breakpoint();
}

fn foo() {
    unsafe { breakpoint() };
}

fn main() {
    foo();
}
