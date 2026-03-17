// { dg-additional-options "-frust-compile-until=typecheck" }
#![feature(no_core)]
#![no_core]

struct Struct(i32);

fn struct_pattern(Struct { 0: a }: Struct) {}
