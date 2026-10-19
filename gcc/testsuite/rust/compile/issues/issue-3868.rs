#![feature(no_core, intrinsics, lang_items, staged_api)]
#![feature(rustc_attrs)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

#[rustc_args_required_const(1)]
fn foo<const N: usize>(x: i32) -> i32 {
    x + N as i32
}

fn main() {
    let _a = foo(10, 3);
}
