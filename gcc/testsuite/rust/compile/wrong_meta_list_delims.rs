// { dg-additional-options "-frust-compat-version=1.84" }

// check if the delimiters used for attributes is `(` and not either of `{` `[` 

#![feature(no_core)]
#![no_core]

#[allow{unused}] // { dg-error "wrong meta list delimiters" "" { target *-*-* } }
struct A {}

#[allow[unused]] // { dg-error "wrong meta list delimiters" "" { target *-*-* } }
struct B {}