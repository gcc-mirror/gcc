// { dg-options "-fsyntax-only" }
#![feature(no_core)]
#![no_core]


fn foo() {
    let [] = [0; 0];
}
