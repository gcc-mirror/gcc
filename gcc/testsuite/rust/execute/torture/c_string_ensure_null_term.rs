// { dg-additional-options "-frust-c-style-string-literals" }
#![feature(no_core, intrinsics, staged_api, lang_items, rustc_attrs)]
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

#[rustc_builtin_macro]
macro_rules! cfg_select {
    () => {{}};
}

cfg_select! {
    all(
        not(windows),
        not(target_vendor = "apple"),
        not(target_os = "vita"),
        any(
            target_arch = "aarch64",
            target_arch = "arm",
            target_arch = "csky",
            target_arch = "hexagon",
            target_arch = "msp430",
            target_arch = "powerpc",
            target_arch = "powerpc64",
            target_arch = "riscv32",
            target_arch = "riscv64",
            target_arch = "s390x",
            target_arch = "xtensa",
        )
    ) => {
        pub type c_char = u8;
    }
    _ => {
        pub type c_char = i8;
    }
}

extern "C" {
    fn printf(s: *const c_char, ...);
}

#[lang = "CStr"]
#[repr(transparent)]
pub struct CStr {
    inner: [c_char]
}

impl CStr {
    pub const fn to_ptr(&self) -> *const c_char {
        &self.inner as *const [c_char] as *const c_char
    }
}

pub fn main() -> c_char {
    let a = c"gccrs";
    let val = unsafe { a.to_ptr().add(5) };
    unsafe { *val }
}
