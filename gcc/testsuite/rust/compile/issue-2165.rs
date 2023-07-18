#[lang = "sized"]
pub trait Sized {}

pub trait Alpha<T = Self> {
    type Beta;
}

impl Alpha for u32 {
    type Beta = u32;
}

type Delta = <u32 as Alpha<u32>>::Beta;
