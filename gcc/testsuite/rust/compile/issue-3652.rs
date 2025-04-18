trait Foo {
    type T;
    fn foo() -> T<<Self as Foo>::T>;
    // { dg-error "could not resolve type path .T. .E0412." "" { target *-*-* } .-1 }
}

fn foo() {}
