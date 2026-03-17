#![feature(no_core)]
#![no_core]

use marker::Sized;

pub mod marker {
    pub trait Sized {}
}

pub mod clone {
    pub trait Clone: Sized {}
    // { dg-error "could not resolve type path .Sized." "" { target *-*-* } .-1 }
}
