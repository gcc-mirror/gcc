#![feature(no_core)]
#![no_core]

extern crate extern_crate_dup_1;
extern crate extern_crate_dup_1; // { dg-error ".extern_crate_dup_1. defined multiple times" }
