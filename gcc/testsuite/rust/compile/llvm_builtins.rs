// { dg-options "-mrdrnd" }
#![feature(no_core, abi_unadjusted)]
#![no_core]

#[allow(improper_ctypes)]
extern "unadjusted" {
    #[link_name = "llvm.x86.rdrand.16"]
    fn x86_rdrand16_step() -> (u16, i32);
    #[link_name = "llvm.x86.rdrand.32"]
    fn x86_rdrand32_step() -> (u32, i32);
    #[link_name = "llvm.x86.rdrand.64"]
    fn x86_rdrand64_step() -> (u64, i32);
    #[link_name = "llvm.x86.rdseed.16"]
    fn x86_rdseed16_step() -> (u16, i32);
    #[link_name = "llvm.x86.rdseed.32"]
    fn x86_rdseed32_step() -> (u32, i32);
    #[link_name = "llvm.x86.rdseed.64"]
    fn x86_rdseed64_step() -> (u64, i32);

    #[link_name = "llvm.x86.addcarry.32"]
    fn llvm_addcarry_u32(a: u8, b: u32, c: u32) -> (u8, u32);
    #[link_name = "llvm.x86.addcarryx.u32"]
    fn llvm_addcarryx_u32(a: u8, b: u32, c: u32, d: *mut u8) -> u8;
    #[link_name = "llvm.x86.subborrow.32"]
    fn llvm_subborrow_u32(a: u8, b: u32, c: u32) -> (u8, u32);
    #[link_name = "llvm.x86.addcarry.64"]
    fn llvm_addcarry_u64(a: u8, b: u64, c: u64) -> (u8, u64);
    #[link_name = "llvm.x86.addcarryx.u64"]
    fn llvm_addcarryx_u64(a: u8, b: u64, c: u64, d: *mut u8) -> u8;
    #[link_name = "llvm.x86.subborrow.64"]
    fn llvm_subborrow_u64(a: u8, b: u64, c: u64) -> (u8, u64);

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
    let (_status2, _v1) = unsafe {
        llvm_addcarry_u32(0, 1, 2)
    };
    let (_v2, _status2) = unsafe {
        x86_rdrand16_step()
    };
}
