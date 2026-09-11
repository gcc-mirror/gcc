// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]

fn test() -> f32 {
    return 10000000000000000000000000000000000000000000.0f32;
    // { dg-error "decimal overflows the respective type .f32." "" { target *-*-* } .-1 }
}
