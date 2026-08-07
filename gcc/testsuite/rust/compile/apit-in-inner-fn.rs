#![feature(no_core)]
#![feature(lang_items)]
#![feature(rustc_attrs)]
#![no_core]

#[lang = "sized"]
trait Sized {}

trait Foo {}

pub struct Bar;

impl Bar {
    pub fn foo(b: impl Foo) {
        fn inner(a: impl Foo) {}
    }
}
