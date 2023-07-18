#[lang = "sized"]
pub trait Sized {}

struct Bar;

trait Foo {
    fn bar(&self) {} // { dg-warning "unused name" }
}

pub fn outer() {
    impl Foo for Bar {}
}

fn main() {
    Bar.bar();
}
