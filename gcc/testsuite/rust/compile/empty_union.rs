#![feature(no_core)]
#![no_core]

#[repr(C)]
union MyUnion {} // { dg-error "unions cannot have zero fields" }
