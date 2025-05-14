enum Foo {
    Bar(isize),
}

fn main() {
    match Foo::Bar(205) {
        Foo { i } => (),
        // { dg-error "expected struct, variant or union type, found enum .Foo. .E0574." "" { target *-*-* } .-1 }
    }
}
