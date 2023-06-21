#![feature(intrinsics)]

extern "rust-intrinsic" {
    pub fn rotate_left<T>(l: T, r: T) -> T;
}

fn main() -> i32 {
    let a = 15;
    let b = 15;

    let _ = rotate_left(a, b);

    0
}
