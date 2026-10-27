#![feature(no_core)]
#![feature(intrinsics)]
#![feature(lang_items)]
#![no_core]

#[lang = "sized"]
trait Sized {}

extern "rust-intrinsic" {
    fn ptr_offset_from<T>(ptr: *const T, base: *const T) -> isize;
    fn size_of<T>() -> usize;
}

fn main() -> i32 {
    let value = 0;

    let first = &value as *const i32;

    // Crimes? A lil bit of crimes?
    let last = first as usize;

    let last = last + 8 * size_of::<i32>();

    let first = first as *const i32;
    let last = last as *const i32;

    let offset = unsafe { ptr_offset_from(last, first) };

    offset as i32 - 8
}
