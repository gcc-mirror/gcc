#![feature(no_core)]
#![no_core]

const A: usize = {
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let x = 23;
    x
};

static B: usize = {
    let x = 23;
    x
};
