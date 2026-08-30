// { dg-options "-w" }

#![feature(no_core)]
#![no_core]

struct A {
    b: &'static B,
}

struct B {
    c: &'static C,
}

struct C {
    a: &'static A,
}
