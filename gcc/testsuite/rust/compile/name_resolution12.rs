// { dg-additional-options "-frust-compile-until=lowering" }
#![feature(no_core)]
#![no_core]


const TOTO: i32 = 10;

fn foo() {
    let b = 10;
    fn bar() {
        let e = TOTO;
    }
}
