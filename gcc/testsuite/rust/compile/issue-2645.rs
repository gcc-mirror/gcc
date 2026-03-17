// { dg-additional-options "-frust-compile-until=ast" }
#![feature(no_core)]
#![no_core]

pub fn struct_tuple(A { 0: a, 1: ref b }: A) -> i32 {
    a
}
