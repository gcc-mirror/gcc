// { dg-options "-O2 -w -fdump-tree-optimized" }
#![feature(intrinsics)]

extern "rust-intrinsic" {
    // { dg-final { scan-tree-dump-times "__builtin_eh_pointer" 1 "optimized" } }
    fn r#try(try_fn: fn(_: *mut u8), data: *mut u8, catch_fn: fn(_: *mut u8, _: *mut u8)) -> i32;
}

extern "C" {
    fn try_fn(data: *mut u8);
    fn catch_fn(data: *mut u8, ex: *mut u8);
}

pub fn not_main(d: &mut u8) -> i32 {
    unsafe {
        // { dg-final { scan-tree-dump-times "try_fn" 1 "optimized" } }
        let _: i32 = r#try(try_fn, d, catch_fn);
        // { dg-final { scan-tree-dump-times "catch_fn" 1 "optimized" } }
    }
    42
}
