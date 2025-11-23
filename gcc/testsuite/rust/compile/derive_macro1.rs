#![feature(lang_items)]
#[lang = "sized"]
pub trait Sized {}

#[lang = "clone"]
pub trait Clone {
    fn clone(&self) -> Self;
}

// This warning can be removed once we properly handle implems with #[automatically_derived]
#[derive(Clone)]
pub struct S;

fn main() {
    let s = S;
    let _s_clone = s.clone();
}
