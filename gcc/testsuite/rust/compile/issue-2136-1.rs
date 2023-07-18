#[lang = "sized"]
pub trait Sized {}

pub trait Foo {
    fn foo();
}

impl Foo for u16 {
    fn foo() {
        // { dg-warning "infinite recursion detected" "" { target *-*-* } .-1 }
        <u16 as Foo>::foo()
    }
}

fn main() {
    <u16>::foo();
}
