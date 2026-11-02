#![feature(no_core, lang_items, optin_builtin_traits)]
#![no_core]

// Rust-GCC/gccrs#4971: checking a bound on a struct whose fields are structs
// used to take time and memory exponential in the nesting depth, because every
// probe added every auto trait and checked each of them on every field.

#[lang = "sized"]
pub trait Sized {}

pub unsafe auto trait Send {}
pub unsafe auto trait Sync {}

pub struct L0;
pub struct L1(L0, L0);
pub struct L2(L1, L1);
pub struct L3(L2, L2);
pub struct L4(L3, L3);
pub struct L5(L4, L4);
pub struct L6(L5, L5);
pub struct L7(L6, L6);
pub struct L8(L7, L7);
pub struct L9(L8, L8);
pub struct L10(L9, L9);
pub struct L11(L10, L10);
pub struct L12(L11, L11);

pub trait T {}
impl T for L12 {}

pub fn need<X: T>(_x: X) {}

pub fn f(x: L12) {
    need(x);
}
