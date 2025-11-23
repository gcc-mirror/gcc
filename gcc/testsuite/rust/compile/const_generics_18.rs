#![feature(lang_items)]
#[lang = "sized"]
trait Sized {}

struct Foo<const N: usize>;
type Alias = Foo<4>;

fn main() -> i32 {
    let _x: Alias = Foo::<4> {};
    0
}
