// { dg-options "-w" }
#![feature(lang_items)]
#[lang = "sized"]
trait Sized {}

const BASE: usize = 2;

struct Foo<T, const N: usize> {
    data: [T; N],
}

fn main() {
    let _ = Foo::<u8, { BASE + 1 * 2 }> { data: [0; 4] };
}
