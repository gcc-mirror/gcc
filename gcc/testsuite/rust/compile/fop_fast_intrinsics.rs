#![feature(no_core)]
#![feature(intrinsics)]
#![feature(lang_items)]
#![no_core]

#[lang = "sized"]
trait Sized {}

extern "rust-intrinsic" {
    fn fadd_fast<T>(a: T, b: T) -> T;
    fn fsub_fast<T>(a: T, b: T) -> T;
    fn fmul_fast<T>(a: T, b: T) -> T;
    fn fdiv_fast<T>(a: T, b: T) -> T;
    fn frem_fast<T>(a: T, b: T) -> T;
}

fn main() {
    unsafe {
        fadd_fast(4.07, 2.9);
        fsub_fast(4.07, 2.9);
        fmul_fast(4.07, 2.9);
        fdiv_fast(4.07, 2.9);

        frem_fast(4.07f32, 2.9f32);
        frem_fast(4.07f64, 2.9f64);
    }
}
