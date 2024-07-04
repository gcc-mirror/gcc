#[lang = "sized"]
pub trait Sized {}

struct Bar;

trait Foo {
    const N: u32;

    fn M();
}

impl Foo for Bar {
    // { dg-error "missing N, M in implementation of trait .Foo." "" { target *-*-* } .-1 }
    fn N() {}
    // { dg-error "method .N. is not a member of trait .Foo." "" { target *-*-* } .-1 }
}
