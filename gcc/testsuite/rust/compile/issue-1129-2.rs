// { dg-additional-options "-w" }
pub trait Hasher {
    fn finish(&self) -> u64;
    fn write(&mut self, bytes: &[u8]);
    fn write_u8(&mut self, i: u8) {
        self.write(&[i])
    }
}

struct SipHasher;

impl Hasher for SipHasher {
    #[inline]
    fn write(&mut self, msg: &[u8]) {
        loop {}
    }

    #[inline]
    fn finish(&self) -> u64 {
        0
    }
}
