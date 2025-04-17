enum Foo {
    Bar,
}

struct Baz;

fn main() {
    Foo::Bar.a;
    // { dg-error "no field .a. on type .Foo. .E0609." "" { target *-*-* } .-1 }
    Baz.a;
    // { dg-error "no field .a. on type .Baz. .E0609." "" { target *-*-* } .-1 }
}
