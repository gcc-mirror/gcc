// { dg-additional-options "-fsyntax-only" }
#![feature(no_core)]
#![no_core]


extern "C" {
    type F;
    type E // { dg-error "failed to parse" }
} // { dg-error "expecting" }
