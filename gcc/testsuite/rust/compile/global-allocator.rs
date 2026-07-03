#![feature(no_core)]
#![no_core]

// TODO: there should be a check for X: GlobalAlloc
// TODO: also, should this attribute be available with no_core?
#[global_allocator]
static A: u32 = 0;

#[global_allocator()]
static B: u32 = 0;
// { dg-error "malformed .global_allocator. attribute" "" { target *-*-* } .-2 }

#[global_allocator]
const C: u32 = 0;
// { dg-error "only be applied to static" "" { target *-*-* } .-2 }
