#[lang = "sized"]
pub trait Sized {}

trait Foo {
    fn method(&self);
}

struct Bar;
impl Foo for Bar {}

fn main() {
    let x: impl Foo = Bar; // { dg-error ".impl Trait. not allowed outside of function and inherent method return types .E0562." }

    struct Wrapper {
        field: impl Foo, // { dg-error ".impl Trait. not allowed outside of function and inherent method return types .E0562." }
    }
}
