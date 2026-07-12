// { dg-additional-options "-frust-compile-until=typecheck" }
#![feature(no_core, extern_types)]
#![no_core]

extern "C" {
    type X;
}

type Y = X;
