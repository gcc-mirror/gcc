// { dg-options "-w" }

#![feature(no_core)]
#![no_core]

struct A {
    b: B,
}

struct B {
    a: *const A,
}

struct Node {
    parent: *const Node,
    next: *mut Node,
}
