#![feature(lang_items)]

#[lang = "sized"]
trait Sized {}

pub struct MyBuf;

trait Foo {
    type Bar<T>: Sized;
}

impl Foo for MyBuf {
    type Bar<T> = T;
}

type A = <MyBuf as Foo>::Bar<u32>;
fn main() -> i32 {
    let a: A = 1;
    a as i32 - 1
}
