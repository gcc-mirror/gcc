#![feature(min_specialization)]

#[lang = "sized"]
trait Sized {}

trait Foo {
    fn foo(&self) -> i32;
}

impl<T> Foo for T {
    default fn foo(&self) -> i32 { // { dg-warning "unused" }
        15
    }
}

impl Foo for bool {
    fn foo(&self) -> i32 {
        if *self {
            1
        } else {
            0
        }
    }
}

fn main() -> i32 {
    let a = 1.foo() - 15;
    let b = true.foo() - 1;

    a + b
}
