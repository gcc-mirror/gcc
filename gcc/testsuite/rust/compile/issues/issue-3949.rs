#![feature(no_core)]
#![no_core]

const fn a() -> usize {
    b()
}

const fn b() -> usize {
    a()
}

const ARR: [i32; a()] = [5; 6];
// { dg-error "mismatched types" "" { target *-*-* } .-1 }
// { dg-error "evaluation depth exceeds maximum" "" { target *-*-* } 0 }
