// { dg-do compile { target { { i?86-*-* x86_64-*-* } && { ! ia32 } } } }
// { dg-options "-mrdrnd -m64 -mrdseed -fdump-tree-gimple" }
#![feature(no_core, abi_unadjusted)]
#![no_core]

#[allow(improper_ctypes)]
extern "unadjusted" {
    #[link_name = "llvm.x86.rdrand.64"]
    fn x86_rdrand64_step() -> (u64, i32);
    #[link_name = "llvm.x86.rdseed.64"]
    fn x86_rdseed64_step() -> (u64, i32);

    #[link_name = "llvm.x86.addcarry.64"]
    fn llvm_addcarry_u64(a: u8, b: u64, c: u64) -> (u8, u64);
    #[link_name = "llvm.x86.addcarryx.u64"]
    fn llvm_addcarryx_u64(a: u8, b: u64, c: u64, d: *mut u8) -> u8;
    #[link_name = "llvm.x86.subborrow.64"]
    fn llvm_subborrow_u64(a: u8, b: u64, c: u64) -> (u8, u64);
}

fn main() {
    let (_v1, _s1) = unsafe {
        x86_rdrand64_step() // { dg-final { scan-tree-dump-times {__builtin_ia32_rdrand64_step} 1 gimple } }
    };
    let (_v2, _s2) = unsafe {
        x86_rdseed64_step() // { dg-final { scan-tree-dump-times {__builtin_ia32_rdseed_di_step} 1 gimple } }
    };

    // { dg-final { scan-tree-dump-times {__builtin_ia32_addcarryx_u64} 2 gimple } }
    let (_s3, _v3) = unsafe {
        llvm_addcarry_u64(0, 1, 2)
    };
    let _s4 = unsafe {
        let _temp_ptr: *mut u8;
        llvm_addcarryx_u64(0, 1, 2, _temp_ptr)
    };

    // { dg-final { scan-tree-dump-times {__builtin_ia32_sbb_u64} 1 gimple } }
    let (_s5, _v5) = unsafe {
        llvm_subborrow_u64(0, 1, 2)
    };
}
