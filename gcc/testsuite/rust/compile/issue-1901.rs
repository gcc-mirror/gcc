#![feature(intrinsics)]

#[lang = "sized"]
pub trait Sized {}

mod intrinsics {
    extern "rust-intrinsic" {
        pub fn offset<T>(ptr: *const T, count: isize) -> *const T;
    }
}

mod ptr {
    #[lang = "const_ptr"]
    impl<T> *const T {
        pub unsafe fn offset(self, count: isize) -> *const T {
            crate::intrinsics::offset(self, count)
        }
    }

    #[lang = "mut_ptr"]
    impl<T> *mut T {
        pub unsafe fn offset(self, count: isize) -> *mut T {
            crate::intrinsics::offset(self, count) as *mut T
        }
    }
}

pub fn test_const(x: *const u8) {
    unsafe {
        x.offset(1);
    }
}

pub fn test_mut(x: *mut u8) {
    unsafe {
        x.offset(1);
    }
}
