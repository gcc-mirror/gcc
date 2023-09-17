#![allow(unused)]
const SOME_CONST: i32 = 12;

fn some_function() {
    SOME_CONST = 15; // { dg-error "assignment of read-only location" }
}

fn main() {
    some_function();
}
