#![feature(no_core)]
#![no_core]

#[cfg_attr(not(wrong = "32"), repr(i32))]
enum Eu64 {
    Au64 = 0,
}