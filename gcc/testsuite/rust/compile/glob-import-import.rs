// { dg-additional-options "-w" }
// TODO: this shouldn't warn

#![feature(no_core)]
#![no_core]

pub mod foo {
    pub struct S;
}

use foo::S;

mod bar {
    use super::*;

    pub const X: S = S;
}

pub const Y: S = bar::X;
