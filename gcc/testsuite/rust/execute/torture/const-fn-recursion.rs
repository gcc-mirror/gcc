// { dg-additional-options "-w" }
#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

const fn fib(n: i32) -> i32 {
    if n < 2 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

const X: i32 = fib(20);

fn main() -> i32 {
    if X == 6765 {
        0
    } else {
        1
    }
}
