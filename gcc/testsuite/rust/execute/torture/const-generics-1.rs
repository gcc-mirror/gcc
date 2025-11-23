#![feature(lang_items)]

#[lang = "sized"]
trait Sized {}

struct Foo<const N: usize>;

impl Foo<1> {
    fn call(&self) -> i32 {
        10
    }
}

impl Foo<2> {
    fn call(&self) -> i32 {
        20
    }
}

fn main() -> i32 {
    let a = Foo::<1> {};
    let b = Foo::<2> {};
    let aa = a.call();
    let bb = b.call();
    bb - aa - 10
}
