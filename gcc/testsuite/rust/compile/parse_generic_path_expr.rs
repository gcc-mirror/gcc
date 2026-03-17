// { dg-additional-options "-frust-compile-until=ast" }
#![feature(no_core)]
#![no_core]

fn main() {
    only_foo::<<i32 as Bar>::Item>();
}
