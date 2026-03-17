// { dg-options "-w" }
#![feature(no_core)]
#![no_core]

#![feature(lang_items)]
#[lang = "sized"]
trait Sized {}

struct ArrayWrapper<T, const N: usize> {
    data: [T; N],
}

pub fn test() -> [u8; 4] {
    let a = ArrayWrapper { data: [1u8; 4] };
    a.data
}
