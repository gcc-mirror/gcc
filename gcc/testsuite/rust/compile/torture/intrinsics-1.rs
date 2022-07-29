// { dg-additional-options -fdump-tree-original }

#![feature(intrinsics)]

extern "rust-intrinsic" {
    pub fn sqrtf32(x: f32) -> f32;
    pub fn sinf32(x: f32) -> f32;
}

fn main() {
    unsafe fn foo() {
        let mut f32;

        f32 = sqrtf32(5f32);
        // { dg-final { scan-tree-dump-times {(?n)f32 = __builtin_sqrtf \(5\.0e\+0\);$} 1 original } }

        f32 = sinf32(39f32);
        // { dg-final { scan-tree-dump-times {(?n)f32 = __builtin_sinf \(3\.9e\+1\);$} 1 original } }
    }

    unsafe { foo() };
}
