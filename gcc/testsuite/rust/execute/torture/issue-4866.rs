#![feature(no_core, lang_items)]
#![no_core]
#[lang = "sized"]
pub trait Sized {}
#[lang = "partial_ord"]
trait PartialOrd<Rhs: ?Sized = Self> {
    fn lt(&self, rhs: &Rhs) -> bool;
}
impl PartialOrd for i8 {
    fn lt(&self, other: &i8) -> bool {
        *self < *other
    }
}
impl<A: ?Sized, B: ?Sized> PartialOrd<&B> for &A
where
    A: PartialOrd<B>,
{
    fn lt(&self, other: &&B) -> bool {
        PartialOrd::lt(*self, *other)
    }
}

fn main() -> i32 {
    let a = 1i8;
    let b = 2i8;
    if !(a < b) || b < a {
        return 1;
    }
    if !(&a < &b) || &b < &a {
        return 2;
    }
    0
}
