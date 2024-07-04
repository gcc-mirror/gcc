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
    // { dg-error "no method named .foo. found in the current scope" "" { target *-*-* } .-1 }
}
