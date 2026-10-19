// { dg-options "-w" }
#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

struct Wrapper<T> {
    t: T,
}

type Alias<U> = Wrapper<U>;

fn f<V> (x: Alias<V>) -> V {
    x.t
}

fn main() {
    let x: u32 = f(Wrapper { t: 1u32 });
}
