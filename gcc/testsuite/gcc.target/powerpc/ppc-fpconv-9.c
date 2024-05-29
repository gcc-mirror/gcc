/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O3 -mdejagnu-cpu=power7 -ffast-math" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-final { scan-assembler-times {\mfctidz\M|\mxscvdpsxds\M} 2 } } */
/* { dg-final { scan-assembler-not   {\mlwz\M} } } */
/* { dg-final { scan-assembler-not   {\mstw\M} } } */
/* { dg-final { scan-assembler-not   {\mld\M}  } } */
/* { dg-final { scan-assembler-not   {\mstd\M} } } */

void float_to_llong  (long long *dest, float  src) { *dest = (long long) src; }
void double_to_llong (long long *dest, double src) { *dest = (long long) src; }
