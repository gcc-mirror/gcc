#![feature(no_core, lang_items, repr_simd)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

#[repr(simd)]
pub struct u32x4(pub u32, pub u32, pub u32, pub u32);

#[repr(simd)]
pub struct i32x4(pub i32, pub i32, pub i32, pub i32);

pub fn make_u32x4(a: u32, b: u32, c: u32, d: u32) -> u32x4 {
    u32x4(a, b, c, d)
}

pub fn make_i32x4(a: i32, b: i32, c: i32, d: i32) -> i32x4 {
    i32x4(a, b, c, d)
}

pub fn pass_u32x4(v: u32x4) -> u32x4 {
    v
}

pub fn first_u32x4(v: u32x4) -> u32 {
    v.0
}

pub fn third_i32x4(v: i32x4) -> i32 {
    v.2
}

pub fn constructed_second_u32x4(a: u32, b: u32, c: u32, d: u32) -> u32 {
    make_u32x4(a, b, c, d).1
}
