#![feature(no_core, lang_items)]
#![no_core]
#[lang = "sized"]
pub trait Sized {}
#[lang = "deref"]
pub trait Deref {
    type Target: ?Sized;
    fn deref(&self) -> &Self::Target;
}
pub struct VaList {
    pub inner: i32,
}
impl Deref for VaList {
    type Target = i32;
    fn deref(&self) -> &i32 {
        &self.inner
    }
}
pub struct Pin<P> {
    pub pointer: P,
}
impl<P: Deref> Pin<P> {
    pub const unsafe fn new_unchecked(pointer: P) -> Pin<P> {
        Pin { pointer }
    }
    pub fn as_ref(&self) -> Pin<&P::Target> {
        unsafe { Pin::new_unchecked(&*self.pointer) }
    }
}
impl<T: ?Sized> Deref for &T {
    type Target = T;
    fn deref(&self) -> &T {
        *self
    }
}
