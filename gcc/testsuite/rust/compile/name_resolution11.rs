// { dg-additional-options "-frust-compile-until=lowering" }
#![feature(no_core)]
#![no_core]

fn foo() {
    let b = 10;
    fn bar() {
        let a = foo;
    }
}
