trait Foo {
    const A: i32;
    // { dg-warning "unused name .Foo::A." "" { target *-*-* } .-1 }

    fn test(self);
    // { dg-warning "unused name .self." "" { target *-*-* } .-1 }
    // { dg-warning "unused name .Foo::test." "" { target *-*-* } .-2 }
}

struct Bar;
impl Foo for Bar {
    const A: i32 = 123;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    fn test(self) {}
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}

fn main() {
    let a = Bar;
    a.test();
}
