// { dg-additional-options "-fdump-tree-gimple -frust-crate=good_name" }
#![feature(no_core)]
#![no_core]

pub fn does_nothing() {}
fn main() {
    does_nothing()
}
// { dg-final { scan-tree-dump-times {good_name::does_nothing} 2 gimple } }
// { dg-final { scan-tree-dump-times {good_name::main} 1 gimple } }
