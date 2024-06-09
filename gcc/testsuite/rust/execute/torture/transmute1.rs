// { dg-additional-options "-w" }
#![feature(intrinsics)]

extern "rust-intrinsic" {
    fn transmute<T, U>(value: T) -> U;
}

#[lang = "sized"]
pub trait Sized {}

struct WrapI {
    inner: i32,
}

struct WrapF {
    inner: f32,
}

fn main() -> i32 {
    let f = 15.4f32;
    let f_wrap = WrapF { inner: f };

    let fst = unsafe { transmute::<f32, i32>(f) };
    let snd = unsafe { transmute::<WrapF, WrapI>(f_wrap) };

    fst - snd.inner
}
