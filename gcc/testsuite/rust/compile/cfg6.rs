// { dg-additional-options "-frust-cfg=A=\"B\"" }
#![feature(no_core)]
#![no_core]

#[cfg(A)]
pub fn foo() {}
pub fn foo() {}
