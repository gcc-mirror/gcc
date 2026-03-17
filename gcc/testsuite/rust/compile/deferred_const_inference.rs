// { dg-additional-options "-frust-compile-until=typecheck" }
#![feature(no_core)]
#![no_core]


// #![feature(generic_arg_infer)]

fn main() {
    let a: [u32; _] = [15u32];
}
