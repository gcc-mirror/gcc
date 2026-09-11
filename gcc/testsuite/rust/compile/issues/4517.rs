#![feature(no_core)]
#![no_core]

const _: () = if true {};
const x: () = if true {}; // { dg-warning "unused name" "" { target *-*-* } }
