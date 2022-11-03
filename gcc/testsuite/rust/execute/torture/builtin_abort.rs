// { dg-shouldfail "abort should stop the program" }
#![feature(rustc_attrs)]
#![feature(intrinsics)]

mod intrinsics {
    extern "rust-intrinsic" {
        pub fn abort() -> !;
    }
}

pub fn main () -> i32 {
    abort();
    0
}
