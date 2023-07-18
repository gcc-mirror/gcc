#[lang = "sized"]
pub trait Sized {}

trait Foo {
    fn bar(&mut self, other: &mut Foo);
}

struct Baz;

impl Foo for Baz {
    fn bar(&mut self, other: &Foo) {}
    // { dg-error "expected" "" { target *-*-* } .-1 }
    // { dg-error "method .bar. has an incompatible type for trait .Foo." "" { target *-*-* } .-2 }
}

fn main() {}
