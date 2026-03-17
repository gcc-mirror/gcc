// { dg-additional-options "-frust-compile-until=ast" }
#![feature(no_core)]
#![no_core]

pub fn foo<D: ::std::fmt::Debug>(_d: D) -> u32 {
    0
}
