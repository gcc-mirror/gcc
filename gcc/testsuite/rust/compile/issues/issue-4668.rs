#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

struct Dummy; // { dg-warning "struct is never constructed" }

impl Dummy {
    const fn func(self) -> usize { // { dg-warning "associated function is never used" }
        42
    }
}

const _: &[usize] = &[0; {
    const DUMMY: &Dummy = &Dummy;
    DUMMY.func()
}];
