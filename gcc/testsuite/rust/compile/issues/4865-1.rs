#![feature(no_core, lang_items)]
#![no_core]
#[lang = "sized"]
pub trait Sized {}
trait Deref {
    type Target: ?Sized;
}
trait PartialOrd<Rhs: ?Sized = Self> {
    fn gt(&self, rhs: &Rhs) -> bool;
}
pub fn compare<P: Deref, Q: Deref>(x: &P::Target, y: &Q::Target) -> bool
where
    P::Target: PartialOrd<Q::Target>,
{
    P::Target::gt(x, y)
}
