// { dg-additional-options "-fdump-tree-gimple" }
#![feature(no_core, lang_items)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

#[lang = "unsafe_cell"]
pub struct UnsafeCell<T> { _v: T }

pub fn normal_ref(_a: &i32) {}

pub fn unsafe_ref(_b: &UnsafeCell<i32>) {}

// { dg-final { scan-tree-dump "normal_ref \\(const i32 & const _a\\)" "gimple" } }
// { dg-final { scan-tree-dump "unsafe_ref \\(struct unsafe_cell::UnsafeCell<i32> & const _b\\)" "gimple" } }
