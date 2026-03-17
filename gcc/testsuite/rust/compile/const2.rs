// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]


const fn foo() {
    const fn bar() {}

    bar();
}
