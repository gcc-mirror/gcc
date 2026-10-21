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

// no output, i32 is inhabited

fn main() -> i32 {
    unsafe { assert_inhabited::<i32>() };

    0
}
