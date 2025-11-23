#![feature(lang_items)]

#[lang = "sized"]
trait Sized {}

trait Magic {
    fn magic(&self) -> usize;
}

struct Foo<const N: usize>;

impl<const N: usize> Magic for Foo<N> {
    fn magic(&self) -> usize {
        N
    }
}

fn main() -> i32 {
    let f = Foo::<7> {};
    let n = f.magic();
    n as i32 - 7
}
