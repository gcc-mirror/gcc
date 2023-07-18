// { dg-additional-options "-w" }
#![feature(intrinsics)]

#[lang = "sized"]
pub trait Sized {}

mod intrinsics {
    extern "rust-intrinsic" {
        pub fn offset<T>(ptr: *const T, count: isize) -> *const T;
    }
}

impl<T> *const T {
    pub unsafe fn offset(self, count: isize) -> *const T {
        unsafe { intrinsics::offset(self, count) }
    }
}

impl<T> [T] {
    pub unsafe fn get_unchecked(&self, index: usize) -> &T {
        unsafe { &*(self as *const [T] as *const T).offset(index as isize) }
    }
}

#[inline]
unsafe fn u8to64_le(buf: &[u8], start: usize, len: usize) -> u64 {
    (unsafe { *buf.get_unchecked(start) } as u64)
}
