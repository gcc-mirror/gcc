#![feature(no_core, intrinsics, staged_api, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

trait Foo<T> {
    fn get(&self) -> T;
}

trait Bar: Foo<i32> + Foo { // { dg-error {generic item takes at least 1 type arguments but 0 were supplied \[E0107\]} }
    fn double_get(&self) -> i32 {
        self.get() + self.get()
    }
}

impl<T> Bar for T where T: Foo<i32> {}

impl Foo<i32> for i32 {
    fn get(&self) -> i32 {
        *self
    }
}

fn main() {
    let x: i32 = 1;
    Bar::double_get(&x);
}
