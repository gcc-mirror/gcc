// { dg-options "-w" }
#![feature(no_core)]
#![no_core]

#[repr(C)] // { dg-error "the ...repr.. attribute may only be applied to structs, enums and unions" }
fn a() {}
