// { dg-options "-w" }

#![feature(no_core)]
#![no_core]

struct A(&'static B);

struct B {
    c: *const C,
}

struct C(&'static A);
