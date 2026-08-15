#![feature(no_core)]
#![no_core]
#![feature(lang_items)]

#[lang = "sized"]
trait Sized {}

pub static FOO: i32 = 42;
pub static BAR: i32 = 42;

pub static BAZ: bool = { (&FOO as *const i32) == (&BAR as *const i32) };

pub fn main() {}
