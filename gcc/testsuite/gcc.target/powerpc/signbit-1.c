/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O2 -mfloat128" } */

int do_signbit_kf (__float128 a) { return __builtin_signbit (a); }
int do_signbit_if (__ibm128 a) { return __builtin_signbit (a); }
int do_signbit_tf (long double a) { return __builtin_signbit (a); }

/* { dg-final { scan-assembler-not   "stxvd2x"  } } */
/* { dg-final { scan-assembler-not   "stxvw4x"  } } */
/* { dg-final { scan-assembler-not   "stxsd"    } } */
/* { dg-final { scan-assembler-not   "stxsdx"   } } */
/* { dg-final { scan-assembler-times "mfvsrd" 3 } } */
/* { dg-final { scan-assembler-times "srdi"   3 } } */
