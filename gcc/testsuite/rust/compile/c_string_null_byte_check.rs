// { dg-additional-options "-frust-c-style-string-literals" }
#![feature(no_core, lang_items, rustc_attrs)]
#![no_core]

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

pub fn main() {
    let _fail = c"gc\0crs";
    // { dg-error "null characters in C string literals are not supported" "" { target *-*-* } .-1 }
}