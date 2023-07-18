#[lang = "sized"]
pub trait Sized {}

trait Foo {
    const A: i32;

    fn test(self);
}

struct Bar;
impl Foo for Bar {}
// { dg-error "missing A, test in implementation of trait .Foo." "" { target *-*-* } .-1 }
