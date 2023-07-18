// { dg-additional-options "-w" }
#[lang = "sized"]
pub trait Sized {}

impl<T> *const T {
    fn test(self) {}
}
