#![feature(no_core, intrinsics, staged_api, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

extern "rust-intrinsic" {
    #[rustc_const_stable(feature = "const_ptr_offset", since = "1.61.0")]
    fn offset<T>(dst: *const T, offset: isize) -> *const T;
}

#[lang = "const_ptr"]
impl<T> *const T {
    pub const unsafe fn offset(self, count: isize) -> *const T {
        unsafe { offset(self, count) } // { dg-error {unknown element size type for .const struct \*const \[i32\].} }
    }
}

fn main() {
    let my_slice: &[i32] = &[1, 2, 3];
    let ptr = unsafe { (my_slice as *const [i32]).offset(0) };
}
