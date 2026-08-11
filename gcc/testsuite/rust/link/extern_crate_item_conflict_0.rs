#![feature(no_core)]
#![no_core]

extern crate extern_crate_item_conflict_1;
mod extern_crate_item_conflict_1 {} // { dg-error ".extern_crate_item_conflict_1. defined multiple times" }
