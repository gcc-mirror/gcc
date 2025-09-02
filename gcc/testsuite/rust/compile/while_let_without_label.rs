// { dg-additional-options "-frust-compile-until=lowering" }

enum Foo {
    A(i32),
}

fn main() {
    let b = Foo::A(15);

    while let Foo::A(x) = b {}
}
