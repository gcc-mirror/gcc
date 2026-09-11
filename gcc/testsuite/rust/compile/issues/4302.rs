#![feature(no_core, intrinsics, staged_api, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

pub struct Foo<const N: usize>;

pub fn foo<const N: usize>() -> Foo<{ N + 1 }> {
    // { dg-error "failed to resolve const expression" "" { target *-*-* } .-1 }
    Foo
}
