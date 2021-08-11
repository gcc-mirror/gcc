trait Foo {
    fn Bar() -> i32 {}
    // { dg-error "expected .i32. got .()." "" { target *-*-* } .-1 }
}

struct Baz;

impl Foo for Baz {
    fn Bar() {}
    // { dg-error "expected .i32. got .()." "" { target *-*-* } .-1 }
    // { dg-error "method .Bar. has an incompatible type for trait .Foo." "" { target *-*-* } .-2 }
}

fn main() {}
