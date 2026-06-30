// { dg-additional-options "-frust-c-style-string-literals" }
// { dg-output "gccrs" }
#![feature(no_core, lang_items)]
#![no_core]

extern "C" {
    fn printf(s: *const u8, ...);
}

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

fn main() -> i32 {
    let a = c"gccrs";
    unsafe {
        printf(a.to_ptr());
    }
    0
}
