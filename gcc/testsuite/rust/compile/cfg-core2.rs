// { dg-additional-options "-frust-cfg=B" }
#![feature(no_core)]
#![no_core]


#[cfg(not(any(A, B)))]
struct Foo0;

#[cfg(not(any(A, C)))]
struct Bar0;

fn main() {
    let a = Foo0; // { dg-error "cannot find value" }
    let a = Bar0;
}
