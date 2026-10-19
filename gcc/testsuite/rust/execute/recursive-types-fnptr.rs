// { dg-additional-options "-w" }
#![feature(no_core, lang_items)]
#![no_core]
#[lang = "sized"]
trait Sized {}

struct Callback { call: fn(Callback) -> Callback, value: i32 }
fn identity(c: Callback) -> Callback { c }
fn main() -> i32 {
    let c = Callback { call: identity, value: 0 };
    let f = c.call;
    let result = f(c);
    result.value
}
