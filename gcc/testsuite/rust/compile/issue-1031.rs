#![feature(intrinsics)]

#[lang = "sized"]
pub trait Sized {}

extern "rust-intrinsic" {
    #[rustc_const_stable(feature = "const_ptr_offset", since = "1.61.0")]
    pub fn offset<T>(dst: *const T, offset: isize) -> *const T;
}

#[lang = "const_ptr"]
impl<T> *const T {
    pub const unsafe fn offset(self, count: isize) -> *const T {
        unsafe { offset(self, count) }
    }

    pub const unsafe fn add(self, count: usize) -> Self {
        unsafe { self.offset(count as isize) }
    }
}
