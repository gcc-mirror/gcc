/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2 -mfloat128" } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target ppc_float128_sw } */
/* { dg-require-effective-target powerpc_vsx } */

int do_signbit_kf (__float128 a) { return __builtin_signbit (a); }
int do_signbit_if (__ibm128 a) { return __builtin_signbit (a); }
int do_signbit_tf (long double a) { return __builtin_signbit (a); }

/* { dg-final { scan-assembler-not   "stxvd2x"  } } */
/* { dg-final { scan-assembler-not   "stxvw4x"  } } */
/* { dg-final { scan-assembler-not   "stxsd"    } } */
/* { dg-final { scan-assembler-not   "stxsdx"   } } */
/* { dg-final { scan-assembler-times "mfvsrd" 3 } } */
/* { dg-final { scan-assembler-times "srdi"   3 } } */
