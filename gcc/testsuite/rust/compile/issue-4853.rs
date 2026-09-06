// { dg-additional-options "-frust-edition=2018" }
#![feature(lang_items, no_core)]
#![no_core]

#[lang = "sized"]
trait Sized {}

trait FloatToInt<Int>: Sized {
    unsafe fn to_int_unchecked(self) -> Int;
}

impl FloatToInt<u32> for f32 {
    unsafe fn to_int_unchecked(self) -> u32 {
        0
    }
}

impl f32 {
    pub unsafe fn to_int_unchecked<Int>(self) -> Int
    where
        Self: FloatToInt<Int>,
    {
        unsafe { FloatToInt::<Int>::to_int_unchecked(self) }
    }
}
