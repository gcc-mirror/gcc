#![feature(intrinsics)]

extern "rust-intrinsic" {
    pub fn wrapping_add<T>(l: T, r: T) -> T;
    pub fn wrapping_sub<T>(l: T, r: T) -> T;
    pub fn wrapping_mul<T>(l: T, r: T) -> T;
}

fn five() -> u8 {
    5
}

fn main() -> u8 {
    let l = 255;
    let r = five();

    let ret0 = unsafe { wrapping_add(l, r) - 4 }; // 4
    let ret1 = unsafe { wrapping_sub(r, l) - 6 }; // 6
    let ret2 = unsafe { wrapping_mul(r, l) - 251 }; // 251

    ret0 + ret1 + ret2
}
