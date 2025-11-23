#![feature(lang_items)]
#[lang = "sized"]
pub trait Sized {}

struct Bar;

trait Foo {
    fn bar(&self) {}
}

pub fn outer() {
    impl Foo for Bar {}
}

fn main() {
    Bar.bar();
}
