// TODO: this should compile cleanly. It currently ICEs because the same crate
// gets lowered twice, see Rust-GCC/gccrs#4724.
// { dg-ice "" }

#![feature(no_core)]
#![no_core]

extern crate extern_crate_alias_1;
extern crate extern_crate_alias_1 as other;
