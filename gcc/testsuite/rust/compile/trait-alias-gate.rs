#![feature(no_core)]
#![no_core]

trait A {}

trait B = A; // { dg-error "trait aliases are experimental" }
