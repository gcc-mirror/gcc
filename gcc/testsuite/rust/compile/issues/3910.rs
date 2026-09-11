#![feature(no_core)]
#![no_core]

struct B<const M: u32> {}

impl<const M: u32> B<M> {
    const M: u32 = M;
}

struct C;

impl<const M: u32> C { // { dg-error "unconstrained type parameter" }
    const USE_M: u32 = M;
}