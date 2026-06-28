// { dg-additional-options "-frust-unused-check-2.0" }
// { dg-skip-if "discriminant needs a 64-bit isize" { ! lp64 } }
#![feature(no_core)]
#![no_core]

#[repr(C)]
pub enum Big {
    A = 9223372036854775807,
// { dg-warning "enum discriminant does not fit into C" "" { target *-*-* } .-1 }
}

#[repr(C)]
pub enum Small {
    B = 5,
}

pub enum NoRepr {
    C = 9223372036854775807,
}
