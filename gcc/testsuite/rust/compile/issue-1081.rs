// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]

enum Foo {
    A,
    B,
}

fn inspect(f: Foo, g: u8) {
    match (f, g) {
        (Foo::A, 1) => {}
        (Foo::B, 2) => {}
        _ => {}
    }
}
