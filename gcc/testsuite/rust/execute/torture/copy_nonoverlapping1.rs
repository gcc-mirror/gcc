#![feature(intrinsics)]
#[lang = "sized"]
pub trait Sized {}

extern "rust-intrinsic" {
    pub fn copy_nonoverlapping<T>(src: *const T, dst: *mut T, count: usize);
}

fn main() -> i32 {
    let i = 15;
    let mut i_copy = 16;

    let i = &i as *const i32;
    let i_copy = &mut i_copy as *mut i32;

    unsafe {
        copy_nonoverlapping(i, i_copy, 1);

        *i_copy - *i
    }
}
