#![feature(lang_items, no_core)]
#![no_core]

#[lang = "sized"]
trait Sized {}

trait PartialEq<Rhs> {}

impl<A, B, const N: usize> PartialEq<[B; N]> for [A; N] {}

fn main() {}
