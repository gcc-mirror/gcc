#![feature(intrinsics)]

#![feature(lang_items)]
#[lang = "sized"]
pub trait Sized {}

extern "rust-intrinsic" {
    fn size_of<T>() -> usize;
    fn offset<T>(dst: *const T, offset: isize) -> *const T;
}
