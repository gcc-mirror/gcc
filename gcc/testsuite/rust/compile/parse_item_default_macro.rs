// { dg-additional-options "-frust-compile-until=ast" }
#![feature(no_core)]
#![no_core]

macro_rules! default {
    ($($x:tt)*) => { $($x)* }
}

default! {
    struct A;
}
