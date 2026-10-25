#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

impl u32 {
    pub const BITS: u32 = 32;
}

fn from_u64(mut v: u64) -> u64 {
    let mut sz: u64 = 0;
    while v > 0 {
        v >>= <u32>::BITS;
        sz += 1;
    }
    sz
}

fn main() {
    let _ = from_u64(5);
}