/* { dg-do compile { target lp64 } } */
/* { dg-options "-mavx -mcall-ms2sysv-xlogues -O2" } */
/* { dg-final { scan-assembler "call.*__avx_savms64_18" } } */
/* { dg-final { scan-assembler "jmp.*__avx_resms64x_18" } } */

void __attribute__((sysv_abi)) a() {
}

static void __attribute__((sysv_abi)) (*volatile a_noinfo)() = a;

void __attribute__((ms_abi)) b() {
  __asm__ __volatile__ ("" :::"rbx", "rbp", "r12", "r13", "r14", "r15");
  a_noinfo ();
}
