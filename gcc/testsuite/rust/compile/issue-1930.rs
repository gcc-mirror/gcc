// { dg-options "-w" }
#![feature(no_core)]
#![no_core]

#![feature(lang_items)]
#[lang = "sized"]
pub trait Sized {}

fn test<T>(x: *mut T) {
    let x = x as *mut u8;
}
