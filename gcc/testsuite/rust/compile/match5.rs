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
        Foo::C(a) => {}
        Foo::D(x, y) => {} // { dg-error "expected tuple struct or tuple variant, found struct variant 'Foo::D'" }
    }
}
