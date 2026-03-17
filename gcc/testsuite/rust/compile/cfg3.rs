// { dg-additional-options "-w -frust-cfg=A -frust-cfg=B" }
#![feature(no_core)]
#![no_core]

struct Foo;
impl Foo {
    #[cfg(all(A, B))]
    fn test(&self) {}
}

fn main() {
    let a = Foo;
    a.test();
}
