// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]

fn test() -> i32 {
    return 10000000000000000000000000000000000000000000;
    // { dg-error "integer overflows the respective type .i32." "" { target *-*-* } .-1 }
}
