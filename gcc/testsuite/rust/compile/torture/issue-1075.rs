// { dg-additional-options "-w" }
#![feature(intrinsics)]

extern "rust-intrinsic" {
    #[rustc_const_stable(feature = "const_ptr_offset", since = "1.61.0")]
    pub fn offset<T>(dst: *const T, offset: isize) -> *const T;
}

struct FatPtr<T> {
    data: *const T,
    len: usize,
}

union Repr<T> {
    rust: *const [T],
    rust_mut: *mut [T],
    raw: FatPtr<T>,
}

impl<T> *const [T] {
    pub const fn len(self) -> usize {
        // SAFETY: this is safe because `*const [T]` and `FatPtr<T>` have the same layout.
        // Only `std` can make this guarantee.
        unsafe { Repr { rust: self }.raw.len }
    }

    pub const fn as_ptr(self) -> *const T {
        self as *const T
    }
}

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
