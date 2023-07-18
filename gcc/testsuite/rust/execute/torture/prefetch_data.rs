#![feature(intrinsics)]

#[lang = "sized"]
pub trait Sized {}

extern "rust-intrinsic" {
    fn prefetch_read_data<T>(addr: *const T, locality: i32);
    fn prefetch_write_data<T>(addr: *const T, locality: i32);
}

fn main() -> i32 {
    let a = [1, 2, 3, 4];

    unsafe {
        prefetch_read_data(&a, 3);
        prefetch_write_data(&a[0], 3);
    }

    0
}
