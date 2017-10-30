/* { dg-do compile { target lp64 } } */
/* { dg-options "-mno-avx -msse -mcall-ms2sysv-xlogues -O2" } */
/* { dg-final { scan-assembler "call.*__sse_savms64f?_12" } } */
/* { dg-final { scan-assembler "jmp.*__sse_resms64f?x_12" } } */

void __attribute__((sysv_abi)) a() {
}

static void __attribute__((sysv_abi)) (*volatile a_noinfo)() = a;

void __attribute__((ms_abi)) b() {
  a_noinfo ();
}
