// { dg-options "-w" }

#![feature(no_core, lang_items)]
#![no_core]
#[lang = "sized"]
pub trait Sized {}
trait Source { type Item; }
trait Marker { fn check(&self); }
fn bounded<T: Source>(x: &T::Item) where T::Item: Marker {
    T::Item::check(x);
}
fn unbounded<T: Source>(x: &T::Item) {
    T::Item::check(x); // { dg-error "failed to resolve path segment" }
}
