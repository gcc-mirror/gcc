// { dg-additional-options "-frust-compile-until=typecheck" }
#![feature(no_core)]
#![no_core]

#![feature(lang_items)]
#[lang = "sized"]
pub trait Sized {}

#[lang = "copy"]
pub trait Copy {}

#[lang = "clone"]
pub trait Clone {
    fn clone(&self) -> Self;
}

#[lang = "phantom_data"]
pub struct PhantomData<T>;

impl<T: ?Sized> Clone for PhantomData<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized> Copy for PhantomData<T> {}

#[derive(Copy, Clone)]
struct S<const N: usize> {
    x: PhantomData<[u8; N]>
}
