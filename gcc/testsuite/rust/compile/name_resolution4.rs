#[lang = "sized"]
pub trait Sized {}

trait Foo {
    fn foo(&self) {} // { dg-warning "unused name" }
}

struct Bar;

pub fn bar() {
    impl Foo for Bar {}
}

fn main() {
    Bar.foo();
}
