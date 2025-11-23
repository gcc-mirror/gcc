#![feature(lang_items)]
#[lang = "sized"]
trait Sized {}

struct Foo<T, const N: usize> {
    value: [T; N],
}

fn main() {
    let foo: Foo<_, _>;
    // { dg-error {type provided when a constant was expected .E0747.} "" { target *-*-* } .-1 }
}
