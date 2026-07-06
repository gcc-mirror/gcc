#![feature(no_core)]
#![no_core]

#[repr(InvalidRepr)] // { dg-error "unrecognized representation hint" }
struct Foo {
    x: i32,
}

#[repr(align)] // { dg-error "invalid .repr.align.. attribute: .align. needs an argument" }
struct Bar {
    x: i32,
}

#[repr(align(3))] // { dg-error "invalid .repr.align.. attribute: not a power of two" }
struct Baz {}
