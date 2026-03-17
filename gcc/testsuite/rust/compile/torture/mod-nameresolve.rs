#![feature(no_core)]
#![no_core]

mod foo {
    struct A; // { dg-warning "struct is never constructed" }
}

fn main() {}
