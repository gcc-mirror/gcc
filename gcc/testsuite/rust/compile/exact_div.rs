// { dg-additional-options "-fdump-tree-gimple" }

#![feature(no_core)]
#![feature(intrinsics)]
#![feature(lang_items)]
#![no_core]

// Scan for the exact division gimple operator
// { dg-final { scan-tree-dump-times "x /.ex. y" 1 gimple } }

#[lang = "sized"]
trait Sized {}

extern "rust-intrinsic" {
    fn exact_div<T>(x: T, y: T) -> T;
}

fn main() {
    let b = 15;

    let _ = unsafe { exact_div(b, 5) };
}
