// { dg-additional-options "-w" }

#![feature(intrinsics)]

#[lang = "sized"]
pub trait Sized {}

mod mem {
    extern "rust-intrinsic" {
        pub fn transmute<U, V>(_: U) -> V;
    }
}

pub trait Hasher {
    fn write(&mut self, bytes: &[u8]);
    fn write_u16(&mut self, i: u16) {
        self.write(unsafe { &mem::transmute::<_, [u8; 2]>(i) })
    }
}

pub struct SipHasher;

impl Hasher for SipHasher {
    #[inline]
    fn write(&mut self, msg: &[u8]) {}
}
