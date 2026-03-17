// { dg-shouldfail "abort should stop the program" }
#![feature(no_core)]
#![no_core]

#![feature(rustc_attrs)]
#![feature(intrinsics)]

mod intrinsics {
    extern "rust-intrinsic" {
        pub fn abort() -> !;
    }
}

pub fn main () -> i32 {
    intrinsics::abort();
    0
}
