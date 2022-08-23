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
        Foo::C(x) => {}
        Foo::D { z } => {} // { dg-error "variant D does not have a field named z" }
                           // { dg-error "pattern does not mention fields x, y" "" { target *-*-* } .-1 }
    }
}
