// { dg-additional-options "-w" }
mod mem {
    extern "rust-intrinsic" {
        fn transmute<U, V>(_: U) -> V;
    }
}

pub trait Hasher {
    fn write(&mut self, bytes: &[u8]);
    fn write_u16(&mut self, i: u16) {
        self.write(&mem::transmute::<_, [u8; 2]>(i))
    }
}

pub struct SipHasher;

impl Hasher for SipHasher {
    #[inline]
    fn write(&mut self, msg: &[u8]) {}
}
