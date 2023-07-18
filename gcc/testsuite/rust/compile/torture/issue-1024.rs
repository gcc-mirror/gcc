#![feature(intrinsics)]

#[lang = "sized"]
pub trait Sized {}

extern "rust-intrinsic" {
    pub fn size_of<T>() -> usize;
}

fn test() -> usize {
    unsafe { size_of::<i32>() }
}

fn main() {
    let _a = test();
}
