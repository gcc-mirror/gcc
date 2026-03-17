#![feature(no_core)]
#![no_core]

#![feature(no_core)]
#![no_core]

struct Marker;

struct Foo {
    a: Marker,
}

pub mod foo {
    use crate::Marker;
    pub struct Foo {
        b: Marker,
    }
}

use foo::Foo; // { dg-error ".Foo. defined multiple times" }
