#[lang = "sized"]
pub trait Sized {}

pub trait Clone {
    fn clone(&self) -> Self;
}

// This warning can be removed once we properly handle implems with #[automatically_derived]
#[derive(Clone)] // { dg-warning "unused name .self." }
pub struct S;

fn main() {
    let s = S;
    let _s_clone = s.clone();
}
