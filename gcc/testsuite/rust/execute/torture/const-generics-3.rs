#![feature(lang_items)]
#[lang = "sized"]
pub trait Sized {}

fn simd_shuffle<const N: usize>(idx: [u32; N]) -> [u32; N] {
    idx
}

fn main() -> i32 {
    let a = [1u32, 2, 3, 4];
    let out = simd_shuffle(a);
    let _check: [u32; 4] = out;
    0
}
