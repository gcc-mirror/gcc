#![feature(no_core)]
#![no_core]

extern "C" {
    fn variadic(x: isize, args: ...);
}

fn main() {}
