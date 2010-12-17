/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O3 -mcpu=power7 -ffast-math" } */
/* { dg-final { scan-assembler-not "lwz" } } */
/* { dg-final { scan-assembler-not "stw" } } */
/* { dg-final { scan-assembler-not "ld " } } */
/* { dg-final { scan-assembler-not "std" } } */

void float_to_llong  (long long *dest, float  src) { *dest = (long long) src; }
void double_to_llong (long long *dest, double src) { *dest = (long long) src; }
