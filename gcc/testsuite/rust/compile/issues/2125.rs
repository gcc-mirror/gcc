// { dg-timeout 5 }
#![feature(no_core)]
#![no_core]

struct S<$>; // { dg-error ".*" }
