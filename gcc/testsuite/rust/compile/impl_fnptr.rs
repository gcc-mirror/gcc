#![feature(lang_items)]
#[lang = "sized"]
pub trait Sized {}

#[lang = "eq"]
pub trait PartialEq<Rhs: ?Sized = Self> {
    fn eq(&self, other: &Rhs) -> bool;

    fn ne(&self, other: &Rhs) -> bool {
        !self.eq(other)
    }
}

impl<Ret> PartialEq for extern "C" fn() -> Ret {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        *self as usize == *other as usize
    }
}
