#![feature(no_core)]
#![no_core]

mod foo {
    pub struct S;
}

use foo::S as T;

const V: T = T; // { dg-warning "unused name" }
