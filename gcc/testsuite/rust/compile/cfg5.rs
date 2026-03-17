// { dg-additional-options "-w -frust-cfg=A=\"B\"" }
#![feature(no_core)]
#![no_core]

struct Foo;
impl Foo {
    #[cfg(A = "B")]
    fn test(&self) {}
}

fn main() {
    let a = Foo;
    a.test();
}
