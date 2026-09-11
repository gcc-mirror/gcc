#![feature(lang_items, no_core)]
#![no_core]

#[lang = "sized"]
trait Sized {}

trait Deref {
    type Target;
}

trait Marker {}

pub fn check<P>()
where
    P: Deref,
    P::Target: Marker,
{
}
