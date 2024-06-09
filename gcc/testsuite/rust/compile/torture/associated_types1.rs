#[lang = "sized"]
pub trait Sized {}

pub trait Foo {
    type A;

    fn boo(&self) -> <Self as Foo>::A;
}

fn foo2<I: Foo>(x: I) {
    // { dg-warning "function is never used: .foo2." "" { target *-*-* } .-1 }
    x.boo();
}

pub fn main() {}
