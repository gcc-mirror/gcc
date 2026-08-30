#![feature(no_core, extern_types)]
#![no_core]

extern "C" {
    type Opaque;
}

fn takes_ptr(_ptr: *const Opaque) {}

fn main() {
    let ptr = 0 as *const Opaque;
    takes_ptr(ptr);
}
