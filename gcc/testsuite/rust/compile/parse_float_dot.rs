// floating point literals can't start with a '.'
// TODO: improve the error message emitted here
#![feature(no_core)]
#![no_core]

const X: f32 = .5; // { dg-error ".*" }
