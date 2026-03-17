// { dg-additional-options "-frust-compile-until=lowering" }
#![feature(no_core)]
#![no_core]


enum Foo {
    A(i32),
}

fn main() {
    let b = Foo::A(15);

    while let Foo::A(x) = b {}
}
