#![feature(intrinsics)]

#[lang = "sized"]
pub trait Sized {}

extern "rust-intrinsic" {
    pub fn wrapping_add<T>(l: T, r: T) -> T;
}

fn five() -> u8 {
    5
}

fn main() -> u8 {
    let l = 255;
    let r = five();

    unsafe { wrapping_add(l, r) - 4 }
}
