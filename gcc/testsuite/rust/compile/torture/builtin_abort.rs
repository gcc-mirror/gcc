// { dg-options "-fdump-tree-original"  }

// { dg-final { scan-assembler-not "__builtin_abort\[^\"\]" } }
// { dg-final { scan-tree-dump "__builtin_abort" "original" } }

#![feature(rustc_attrs)]
#![feature(intrinsics)]

mod intrinsics {
    extern "rust-intrinsic" {
        pub fn abort() -> !;
    }
}

pub fn main() -> i32 {
    crate::intrinsics::abort();
    0
}
