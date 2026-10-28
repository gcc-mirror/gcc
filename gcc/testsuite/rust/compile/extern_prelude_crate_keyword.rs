#![feature(no_core)]
#![no_core]

use ::crate::Grey as Gray;
// { dg-error "global paths cannot start with .crate." "" { target *-*-* } .-1 }

pub struct Grey;
