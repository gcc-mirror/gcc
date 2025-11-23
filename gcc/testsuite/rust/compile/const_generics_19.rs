#![feature(lang_items)]
#[lang = "sized"]
trait Sized {}

struct Foo<const N: usize>;
struct Wrapper<T>(T);

fn main() -> i32 {
    let _: Wrapper<Foo<3>> = Wrapper(Foo::<3> {});
    0
}
