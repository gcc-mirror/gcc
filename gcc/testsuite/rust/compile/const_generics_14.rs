#![feature(lang_items)]
#[lang = "sized"]
trait Sized {}

type MyLen = usize;
struct Foo<T, const N: usize> {
    data: [T; N],
}

fn main() {
    let _ = Foo::<u8, MyLen> { data: [1, 2, 3] };
    // { dg-error {type provided when a constant was expected .E0747.} "" { target *-*-* } .-1 }
    // { dg-error {expected an ADT type for constructor} "" { target *-*-* } .-2 }
}
