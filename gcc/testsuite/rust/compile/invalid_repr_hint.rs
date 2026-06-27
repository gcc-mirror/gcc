#![feature(no_core)]
#![no_core]

#[repr(InvalidRepr)] // { dg-error "unrecognized representation hint" }
struct Foo {
    x: i32,
}

#[repr(align)] // { dg-error "unrecognized representation hint" }
struct Bar {
    x: i32,
}
