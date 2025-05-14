#![feature(min_specialization)]

#[lang = "sized"]
trait Sized {}

trait Foo {
    fn foo(&self) -> i32;
}

struct Wrap<T>(T);

impl<T> Foo for T {
    default fn foo(&self) -> i32 {
        15
    }
}

impl<T> Foo for Wrap<T> {
    default fn foo(&self) -> i32 {
        16
    }
}

impl Foo for Wrap<bool> {
    fn foo(&self) -> i32 {
        if self.0 {
            1
        } else {
            0
        }
    }
}

fn main() -> i32 {
    Wrap(true).foo() - 1
}
