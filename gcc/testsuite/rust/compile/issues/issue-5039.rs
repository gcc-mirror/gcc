#![feature(no_core, lang_items)]
#![no_core]

// Rust-GCC/gccrs#5039: applying an operator to an error-typed value crashed
// while resolving the operator overload.

#[lang = "sized"]
pub trait Sized {}

#[lang = "mul"]
pub trait Mul<Rhs> {
    type Output;
    fn mul(self, rhs: Rhs) -> Self::Output;
}

impl Mul<i32> for i32 {
    type Output = i32;
    fn mul(self, rhs: i32) -> i32 {
        self
    }
}

pub fn f(v: &[u32]) {
    let _ = v.nope() * 2; // { dg-error "no method named .nope." }
}
