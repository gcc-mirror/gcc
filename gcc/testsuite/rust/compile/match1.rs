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
        Foo::C(a, b) => {}
        // { dg-error "this pattern has 2 fields but the corresponding tuple variant has 1 field" "" { target *-*-* } .-1 }
        Foo::D { x, y } => {}
    }
}
