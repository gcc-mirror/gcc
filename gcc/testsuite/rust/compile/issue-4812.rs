#![no_core]
#![feature(no_core)]
#![feature(lang_items)]
#![feature(min_const_generics)]

#[lang = "sized"]
trait Sized {}

trait Array {
    type Element;
}

impl<T, const N: usize> Array for [T; N] {
    type Element = T;
}

fn require_byte_array<T: Array<Element = u8>>() {}

fn main() {
    require_byte_array::<[u8; 0]>();
    require_byte_array::<[u8; 32]>();
    require_byte_array::<[u8; 1024]>();
}
