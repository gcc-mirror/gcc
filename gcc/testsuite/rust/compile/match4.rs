enum Foo {
    A,
    B,
    C(char),
    D { x: i64, y: i64 },
}

fn inspect(f: Foo) {
    match f {
        Foo::A => {}
        Foo::B => {}
        Foo::C { a } => {}
        // { dg-error "expected struct variant, found tuple variant C" "" { target *-*-* } .-1 }
        Foo::D { x, y } => {}
    }
}
