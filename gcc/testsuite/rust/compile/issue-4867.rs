// { dg-options "-w" }
#![feature(intrinsics, staged_api)]
#![feature(no_core, lang_items)]
#![no_core]
#[lang = "sized"]
pub trait Sized {}
const X: () = trigger();
extern "rust-intrinsic" {
    #[rustc_const_stable(feature = "const_transmute", since = "1.0.0")]
    fn transmute<T, U>(x: T) -> U;
}
struct Arg {
    formatter: fn(&i8),
    value: &'static i8,
}
const fn new<T>(x: &'static T, f: fn(&T)) -> Arg {
    unsafe {
        Arg {
            formatter: transmute(f),
            value: transmute(x),
        }
    }
}
const fn consume(x: &[Arg]) {}
const fn trigger() {
    consume({ &[new(&1i8, display)] });
}
fn display(x: &i8) {}
