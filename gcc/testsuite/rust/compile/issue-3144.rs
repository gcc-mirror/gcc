#[lang = "sized"]
pub trait Sized {}

#[lang = "copy"]
trait Copy {}

#[lang = "clone"]
pub trait Clone {
    fn clone(&self) -> Self;
}

impl Clone for i32 {
    fn clone(&self) -> i32 {
        *self
    }
}

struct S {}

#[derive(Clone, Copy)]
// { dg-error {bounds not satisfied for S .Clone. is not satisfied .E0277.} "" { target *-*-* } .-1 }
struct S2 {
    a: i32,
    s: S,
}

fn main() -> i32 {
    0
}
