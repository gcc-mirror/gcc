#![feature(intrinsics)]

#[lang = "sized"]
pub trait Sized {}

extern "C" {
    fn printf(s: *const i8, ...);
}

mod intrinsics {
    extern "rust-intrinsic" {
        #[rustc_const_stable(feature = "const_ptr_offset", since = "1.61.0")]
        pub fn offset<T>(dst: *const T, offset: isize) -> *const T;
    }
}

#[lang = "mut_ptr"]
impl<T> *mut T {
    pub const unsafe fn offset(self, count: isize) -> *mut T {
        unsafe { intrinsics::offset(self, count) as *mut T }
    }

    pub const unsafe fn add(self, count: usize) -> Self {
        unsafe { self.offset(count as isize) }
    }
}

#[lang = "const_ptr"]
impl<T> *const T {
    pub const unsafe fn offset(self, count: isize) -> *mut T {
        unsafe { intrinsics::offset(self, count) as *mut T }
    }

    pub const unsafe fn add(self, count: usize) -> Self {
        unsafe { self.offset(count as isize) }
    }
}

fn main() -> i32 {
    let a: *mut _ = &mut 123;
    unsafe {
        let _b = a.add(123);
    }

    0
}
