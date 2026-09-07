// { dg-options "-w" }
#![feature(no_core, lang_items, intrinsics, staged_api)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

pub const FLAG: bool = check();

pub const fn check() -> bool {
    return 8i8.wrapping_shr(1) == 4;
}

extern "rust-intrinsic" {
    #[rustc_const_stable(feature = "const_int_unchecked", since = "1.40.0")]
    fn unchecked_shr<T>(x: T, y: T) -> T;
}

impl i8 {
    pub const fn wrapping_shr(self, rhs: u32) -> Self {
        unsafe { unchecked_shr(self, (rhs & 7) as i8) }
    }
}

impl i16 {
    pub const fn wrapping_shr(self, rhs: u32) -> Self {
        unsafe { unchecked_shr(self, (rhs & 15) as i16) }
    }
}
