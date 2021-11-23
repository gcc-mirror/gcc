pub trait Foo {
    type A;

    fn boo(&self) -> <Self as Foo>::A;
    // { dg-warning "unused name .self." "" { target *-*-* } .-1 }
}

fn foo2<I: Foo>(x: I) {
    // { dg-warning "function is never used: .foo2." "" { target *-*-* } .-1 }
    // { dg-warning "unused name .foo2." "" { target *-*-* } .-2 }
    x.boo();
}

pub fn main() {}
