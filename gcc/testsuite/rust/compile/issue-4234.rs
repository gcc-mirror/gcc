// { dg-options "-w" }
#![feature(no_core)]
#![no_core]

#[target_feature(sse)] // { dg-error "attribute can only be applied" }
fn foo() {}