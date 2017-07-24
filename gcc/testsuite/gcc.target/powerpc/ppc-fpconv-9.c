/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-O3 -mcpu=power7 -ffast-math" } */
/* { dg-final { scan-assembler-times {\mfctidz\M|\mxscvdpsxds\M} 2 } } */
/* { dg-final { scan-assembler-not   {\mlwz\M} } } */
/* { dg-final { scan-assembler-not   {\mstw\M} } } */
/* { dg-final { scan-assembler-not   {\mld\M}  } } */
/* { dg-final { scan-assembler-not   {\mstd\M} } } */

void float_to_llong  (long long *dest, float  src) { *dest = (long long) src; }
void double_to_llong (long long *dest, double src) { *dest = (long long) src; }
