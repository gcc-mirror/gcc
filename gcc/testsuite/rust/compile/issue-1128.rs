#[lang = "sized"]
pub trait Sized {}

pub trait Hasher {
    fn write(&mut self, bytes: &[u8]);
    fn write_u8(&mut self, i: u8) {
        self.write(&[i])
    }
}
