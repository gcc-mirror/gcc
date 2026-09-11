#![feature(no_core, intrinsics, staged_api, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

trait Foo<T> {}
trait Bar: Foo {} // { dg-error {generic item takes at least 1 type arguments but 0 were supplied \[E0107\]} }
impl<T> Bar for T {}
impl Foo<i32> for i32 {}
