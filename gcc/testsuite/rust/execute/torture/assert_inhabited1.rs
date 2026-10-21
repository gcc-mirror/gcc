#![feature(no_core)]
#![feature(intrinsics)]
#![feature(lang_items)]
#![no_core]

// { dg-output "attempted to instantiate uninhabited type `!`\r*\nattempted to instantiate uninhabited type `Nothing`\r*\n" }

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

fn main() -> i32 {
    unsafe { assert_inhabited::<!>() };
    unsafe { assert_inhabited::<Nothing>() };

    0
}
