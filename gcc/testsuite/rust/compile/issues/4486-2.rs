#![feature(no_core, intrinsics, staged_api, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

pub trait Number: NumConv {
    fn from<T: Number>(n: T) -> Self;
}

trait NumberExt: Number {
    fn to_double(&self) -> f64 {
        self.to_float() * 2.0
    }
}

impl<T: Number> NumberExt for T {}

impl Number for f64 {
    fn from<T: Number + NewTrait>(n: T) -> f64 { // { dg-error {bounds not satisfied for f64 .NewTrait. is not satisfied \[E0277\]} }
        n.to_float()
    }
}

pub trait NumConv {
    fn to_float(&self) -> f64;
}

impl NumConv for f64 {
    fn to_float(&self) -> f64 {
        *self
    }
}

pub fn main() {
    let _: f64 = Number::from(0.0f64);
}

trait NewTrait {
    type Assoc;
}
