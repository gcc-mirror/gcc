#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

fn is_123(x: [u32; N]) -> bool { // { dg-error "mismatched types, expected .usize. but got .fn \\(\\) -> \\(\\)." }
    // { dg-error "capacity of array .* is not known at compile time" "" { target *-*-* } .-1 }
    match x {
        [1, 2] => true,
        _ => false,
    }
}

fn N() {}
