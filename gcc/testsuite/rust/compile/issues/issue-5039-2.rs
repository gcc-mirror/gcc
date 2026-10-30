#![feature(no_core, lang_items)]
#![no_core]

// Rust-GCC/gccrs#5039: an error-typed array element crashed when the array
// was coerced to a slice.

#[lang = "sized"]
pub trait Sized {}

pub fn g(_s: &[u32]) {}

pub fn f(v: &[u32]) {
    g(&[v.nope(), 1]); // { dg-error "no method named .nope." }
}
