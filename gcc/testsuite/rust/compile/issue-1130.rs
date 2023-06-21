// { dg-additional-options "-w" }
#![feature(intrinsics)]

mod mem {
    extern "rust-intrinsic" {
        fn size_of<T>() -> usize;
        fn transmute<U, V>(_: U) -> V;
    }
}

impl u16 {
    fn to_ne_bytes(self) -> [u8; mem::size_of::<Self>()] {
        unsafe { mem::transmute(self) }
    }
}

pub trait Hasher {
    fn finish(&self) -> u64;

    fn write(&mut self, bytes: &[u8]);

    fn write_u8(&mut self, i: u8) {
        self.write(&[i])
    }

    fn write_i8(&mut self, i: i8) {
        self.write_u8(i as u8)
    }

    fn write_u16(&mut self, i: u16) {
        self.write(&i.to_ne_bytes())
    }

    fn write_i16(&mut self, i: i16) {
        self.write_u16(i as u16)
    }
}

pub struct SipHasher;

impl Hasher for SipHasher {
    #[inline]
    fn write(&mut self, msg: &[u8]) {}

    #[inline]
    fn finish(&self) -> u64 {
        0
    }
}
