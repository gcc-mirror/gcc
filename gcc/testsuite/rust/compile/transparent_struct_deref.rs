#![feature(no_core, intrinsics, staged_api, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

// below's helper code copied from issue-1232.rs
extern "rust-intrinsic" {
    #[rustc_const_stable(feature = "const_ptr_offset", since = "1.61.0")]
    fn offset<T>(dst: *const T, offset: isize) -> *const T;
}

#[lang = "const_ptr"]
impl<T> *const T {
    pub const unsafe fn offset(self, count: isize) -> *const T {
        unsafe { offset(self, count) }
    }

    pub const unsafe fn add(self, count: usize) -> Self {
        unsafe { self.offset(count as isize) }
    }

    pub const fn as_ptr(self) -> *const T {
        self as *const T
    }
}

#[repr(transparent)]
pub struct Foo {
    inner: i32
}

impl Foo {
    pub const fn to_ptr(&self) -> *const i32 {
        &self.inner as *const i32
    }
}

pub fn main() -> i32 {
    let a = Foo { inner: 67 };
    let val = unsafe { a.to_ptr() };
    unsafe { *val - 67 }
}
