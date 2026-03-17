// { dg-options "-w" }
#![feature(no_core)]
#![no_core]

fn foo() -> S {
    S { a: 15 }
}

struct S {
    a: i32,
}
