#![feature(intrinsics, no_core, lang_items)]
#![no_core]

#[lang = "sized"]
trait Sized {}

extern "rust-intrinsic" {
    fn assert_zero_valid<T>();
}

fn main() {
    unsafe {
        assert_zero_valid::<i32>();
        assert_zero_valid::<&i32>();
    }
}
