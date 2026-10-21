#![feature(no_core)]
#![feature(intrinsics)]
#![feature(lang_items)]
#![no_core]

#[lang = "sized"]
trait Sized {}

#[lang = "panic"]
pub fn panic(s: &'static str) -> ! {
    unsafe {
        puts(s as *const str as *const i8);
    }
}

extern "C" {
    fn puts(s: *const i8);
}

extern "rust-intrinsic" {
    fn assert_inhabited<T>();
}

enum Nothing {}

fn main() {
    unsafe { assert_inhabited::<Nothing>() };
    unsafe { assert_inhabited::<!>() };
    unsafe { assert_inhabited::<i32>() };
}
