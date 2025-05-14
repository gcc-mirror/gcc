#![feature(lang_items)]

#[lang = "sized"]
trait Sized {}

pub struct A<T>(T);

pub trait B {
    type C;
}

// ------
// swap these two items

impl B for i32 {
    type C = Weird<i32>;
}

pub struct Weird<T>(A<(T,)>);

// ------

trait Foo {}

impl Foo for Weird<i32> {}

fn main() {}
