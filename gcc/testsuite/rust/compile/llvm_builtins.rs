// { dg-do compile { target { { i?86-*-* x86_64-*-* } && ia32 } } }
// { dg-options "-mrdrnd -mrdseed -fdump-tree-gimple" }
#![feature(no_core, abi_unadjusted)]
#![no_core]

#[allow(improper_ctypes)]
extern "unadjusted" {
    #[link_name = "llvm.x86.rdrand.16"]
    fn x86_rdrand16_step() -> (u16, i32);
    #[link_name = "llvm.x86.rdrand.32"]
    fn x86_rdrand32_step() -> (u32, i32);
    #[link_name = "llvm.x86.rdseed.16"]
    fn x86_rdseed16_step() -> (u16, i32);
    #[link_name = "llvm.x86.rdseed.32"]
    fn x86_rdseed32_step() -> (u32, i32);

    #[link_name = "llvm.x86.addcarry.32"]
    fn llvm_addcarry_u32(a: u8, b: u32, c: u32) -> (u8, u32);
    #[link_name = "llvm.x86.addcarryx.u32"]
    fn llvm_addcarryx_u32(a: u8, b: u32, c: u32, d: *mut u8) -> u8;
    #[link_name = "llvm.x86.subborrow.32"]
    fn llvm_subborrow_u32(a: u8, b: u32, c: u32) -> (u8, u32);

    // TODO implement the SIMD types (i16x8, f32x4, f32x8) before
    // enabling these tests
    // #[link_name = "llvm.x86.vcvtph2ps.128"]
    // fn llvm_vcvtph2ps_128(a: i16x8) -> f32x4;
    // #[link_name = "llvm.x86.vcvtph2ps.256"]
    // fn llvm_vcvtph2ps_256(a: i16x8) -> f32x8;
    // #[link_name = "llvm.x86.vcvtps2ph.128"]
    // fn llvm_vcvtps2ph_128(a: f32x4, rounding: i32) -> i16x8;
    // #[link_name = "llvm.x86.vcvtps2ph.256"]
    // fn llvm_vcvtps2ph_256(a: f32x8, rounding: i32) -> i16x8;
}

fn main() {
    let (_v1, _s1) = unsafe {
        x86_rdrand16_step() // { dg-final { scan-tree-dump-times {__builtin_ia32_rdrand16_step} 1 gimple } }
    };
    let (_v2, _s2) = unsafe {
        x86_rdrand32_step() // { dg-final { scan-tree-dump-times {__builtin_ia32_rdrand32_step} 1 gimple } }
    };
    let (_v3, _s3) = unsafe {
        x86_rdseed16_step() // { dg-final { scan-tree-dump-times {__builtin_ia32_rdseed_hi_step} 1 gimple } }
    };
    let (_v4, _s4) = unsafe {
        x86_rdseed32_step() // { dg-final { scan-tree-dump-times {__builtin_ia32_rdseed_si_step} 1 gimple } }
    };

    // { dg-final { scan-tree-dump-times {__builtin_ia32_addcarryx_u32} 2 gimple } }
    let (_v5, _s5) = unsafe {
        llvm_addcarry_u32(0, 1, 2)
    };
    let _s6 = unsafe {
        let _temp_ptr: *mut u8;
        llvm_addcarryx_u32(0, 1, 2, _temp_ptr)
    };

    let (_v7, _s7) = unsafe {
        llvm_subborrow_u32(0, 1, 2) // { dg-final { scan-tree-dump-times {__builtin_ia32_sbb_u32} 1 gimple } }
    };
}
