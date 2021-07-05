trait Foo {
    type A;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    type B;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    fn new(a: Self::A, b: Self::B) -> Self;
    // { dg-warning "unused name .a." "" { target *-*-* } .-1 }
    // { dg-warning "unused name .b." "" { target *-*-* } .-2 }
    // { dg-warning "unused name .Foo::new." "" { target *-*-* } .-3 }
}

struct Baz(i32, f32);

impl Foo for Baz {
    type A = i32;
    type B = f32;

    fn new(a: i32, b: f32) -> Self {
        Baz(a, b)
    }
}

fn main() {
    Baz::new(123, 456f32);
}
