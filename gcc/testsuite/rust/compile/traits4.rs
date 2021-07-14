trait Foo {
    const A: i32;

    fn test(self);
}

struct Bar;
impl Foo for Bar {
    // { dg-error "missing A in implementation of trait .Foo." "" { target *-*-* } .-1 }
    fn test(self) {}
}

fn main() {
    let a = Bar;
    a.test();
}
