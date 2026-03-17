#![feature(no_core)]
#![no_core]

fn foo() -> isize {
    0
}

fn main() {
    let a = foo();
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
