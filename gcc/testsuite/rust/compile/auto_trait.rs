#![feature(no_core)]
#![no_core]

auto trait Valid {} // { dg-error "auto traits are experimental and possibly buggy" }
