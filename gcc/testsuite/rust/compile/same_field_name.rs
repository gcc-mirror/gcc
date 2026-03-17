// https://doc.rust-lang.org/error_codes/E0124.html
#![feature(no_core)]
#![no_core]

fn main() {
    struct Foo {
        field1: i32,
        field1: i32, // { dg-error "field .field1. is already declared" }
        field1: i32, // { dg-error "field .field1. is already declared" }
    }
}
