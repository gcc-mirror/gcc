// { dg-options "-w" }
#![feature(lang_items)]
#[lang = "sized"]
pub trait Sized {}

fn test<T>(x: *mut T) {
    let x = x as *mut u8;
}
