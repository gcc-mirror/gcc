// { dg-additional-options "-frust-c-style-string-literals" }
#![feature(no_core, lang_items)]
#![no_core]

type c_char = u8;

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