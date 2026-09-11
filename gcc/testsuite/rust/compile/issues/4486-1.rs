#![feature(no_core, intrinsics, staged_api, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

trait Number {
    fn from<T>(n: T) -> Self;
}

trait NumConv {}

fn main() {
    let _: f64 = Number::from(0.0f64); // { dg-error {bounds not satisfied for f64 .Number. is not satisfied \[E0277\]} }
}
