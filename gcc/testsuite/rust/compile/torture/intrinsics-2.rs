// { dg-additional-options -fdump-tree-original }

#![feature(intrinsics)]

extern "rust-intrinsic" {
    pub fn size_of<T>() -> usize;
}

fn main() -> i32 {
    unsafe fn foo() -> usize {
        let f: f32;

        let s_f32 = size_of::<f32>();
        let s_f64 = size_of::<f64>();
        let s_f32_again = size_of::<f32>();

        s_f32 + s_f64 + s_f32_again
    }

    // useless code, just used for function compilation caching
    unsafe { foo() as i32 }
}
