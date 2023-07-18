#[lang = "sized"]
pub trait Sized {}

pub trait Foo {
    fn foo();
}

impl Foo for u16 {
    fn foo() {
        <u16 as Foo>::foo()
    }
}

fn main() {
    let a: u16 = 123;
    a.foo();
    // { dg-error "failed to resolve method for .foo." "" { target *-*-* } .-1 }
}
