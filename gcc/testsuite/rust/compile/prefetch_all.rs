#![feature(no_core)]
#![feature(intrinsics)]
#![feature(lang_items)]
#![no_core]

#[lang = "sized"]
trait Sized {}

extern "rust-intrinsic" {
    fn prefetch_read_data<T>(data: *const T, locality: i32);
    fn prefetch_write_data<T>(data: *const T, locality: i32);
    fn prefetch_read_instruction<T>(instruction: *const T, locality: i32);
    fn prefetch_write_instruction<T>(instruction: *const T, locality: i32);
}

fn main() {
    let a = 15i32;

    unsafe {
        prefetch_read_data(&a as *const i32, 3);
        prefetch_write_data(&a as *const i32, 3);
        prefetch_read_instruction(&a as *const i32, 3);
        prefetch_write_instruction(&a as *const i32, 3);
    }
}
