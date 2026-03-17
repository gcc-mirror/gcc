// { dg-error "expected .* got .*" "" { target *-*-* } 0 }
#![feature(no_core)]
#![no_core]

fn test1() -> i32 {}

fn main() {
    let call1 = test1();
}
