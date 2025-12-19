#![cfg_attr(not(cg_gcc), feature(intrinsics))]
#![feature(lang_items)]

#[lang = "sized"]
pub trait Sized {}

#[cfg(not(cg_gcc))]
extern "rust-intrinsic" {
    fn copy_nonoverlapping<T>(src: &T, dst: &mut T, count: usize);
}
