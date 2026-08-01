#![feature(no_core, repr_simd)]
#![no_core]

// all simd types declared in stdarch/crates/core_arch/src/x86/mod.rs
#[repr(simd)]
pub struct __m128i(i64, i64);
#[repr(simd)]
pub struct __m128(f32, f32, f32, f32);
#[repr(simd)]
pub struct __m128d(f64, f64);
#[repr(simd)]
pub struct __m256i(i64, i64, i64, i64);
#[repr(simd)]
pub struct __m256(f32, f32, f32, f32, f32, f32, f32, f32);
#[repr(simd)]
pub struct __m256d(f64, f64, f64, f64);
#[repr(simd)]
pub struct __m512i(i64, i64, i64, i64, i64, i64, i64, i64);
#[repr(simd)]
pub struct __m512(
    f32, f32, f32, f32, f32, f32, f32, f32,
    f32, f32, f32, f32, f32, f32, f32, f32,
);
#[repr(simd)]
pub struct __m512d(f64, f64, f64, f64, f64, f64, f64, f64);

// errorneous simd types
#[repr(simd)]
pub struct Foo (f32, f32, f32); // { dg-error "Size of SIMD struct must be a power of 2" }
#[repr(simd)]
pub struct Bar (f32, i32); // { dg-error "SIMD struct fields should be of the same type" }
#[repr(simd)]
pub struct Baz (str); // { dg-error "SIMD vector element type should be a primitive scalar .E0077." }

fn main() {}