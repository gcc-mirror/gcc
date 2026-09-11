#![feature(no_core)]
#![no_core]
enum E {
    A = {
        enum Foo {
            Bar(isize),
            Baz,
        }
        0
    },
}
