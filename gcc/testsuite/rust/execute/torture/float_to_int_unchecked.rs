#![feature(no_core)]
#![feature(intrinsics)]
#![feature(lang_items)]
#![no_core]

#[lang = "sized"]
trait Sized {}

extern "rust-intrinsic" {
    fn float_to_int_unchecked<Float, Int>(value: Float) -> Int;
}

fn main() -> i32 {
    let a = 15.07;

    let b = a as i32;
    let c: i32 = unsafe { float_to_int_unchecked(a) };

    // NOTE: This is only valid for certain floating point values as f32 -> i32 conversions
    // are different for invalid values when using the cast and when using the unchecked intrinsic
    b - c
}
