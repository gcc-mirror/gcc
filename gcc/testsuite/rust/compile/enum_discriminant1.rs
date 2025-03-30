enum Foo {
    Bar = 3 + 12,
}

fn test() -> Foo { // { dg-warning "function is never used" }
    return Foo::Bar;
}