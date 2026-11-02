#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

impl<'a> Thing for &'a u8 {
    type Output = u8;

    fn calc(&mut self) -> u8 {
        **self
    }
}

trait Thing {
    type Output: 'static;

    fn calc(&mut self) -> Self::Output;
}

impl<'a> Thing for &'a u8 {
    type Output = u8;

    fn calc(&mut self) -> u8 {
        **self
    }
}

fn get_thing(x: &u8) -> impl Thing + '_ {
    x
}

fn f(n: &u8) -> impl Thing + 'static { // { dg-error "bounds not satisfied for u8 .Thing. is not satisfied" }
    let mut t = get_thing(n);
    t.calc()
}
