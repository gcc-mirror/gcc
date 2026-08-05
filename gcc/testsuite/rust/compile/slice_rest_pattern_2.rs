// { dg-additional-options "-frust-compile-until=typecheck" }
#![feature(no_core)]
#![no_core]

pub fn foo(x: &[u8]) -> i32 {
    match x {
        [] => 0,
        [1, xs @ ..] => foo (xs),
        [x, ..] => *x as i32
    }
}
