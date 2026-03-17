#![feature(no_core)]
#![no_core]

struct Foo(i32, i32);
// { dg-warning "struct is never constructed" "" { target *-*-* } .-1 }

fn main() {}
