#![feature(no_core)]
#![no_core]

#[repr(transparent)]
struct Foo { // { dg-error "transparent struct needs at most one field with non-trivial size or alignment, but has 2" }
    foo: i32,
    bar: i32
}

#[repr(transparent)]
struct Bar {}

#[repr(transparent)]
struct Baz {
    foo: i32
}
