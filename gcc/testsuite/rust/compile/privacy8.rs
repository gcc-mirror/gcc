#![feature(no_core)]
#![no_core]

pub(crate) struct Foo; // { dg-warning "struct is never constructed" }
