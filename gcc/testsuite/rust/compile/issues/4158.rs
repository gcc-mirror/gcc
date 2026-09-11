#![feature(no_core)]
#![no_core]
struct A<const B: A = 1, A>; // { dg-error "invalid order for generic parameters" }