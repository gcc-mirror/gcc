/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2 -mfloat128" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-require-effective-target ppc_float128_sw } */

int do_signbit_kf (__float128 *a) { return __builtin_signbit (*a); }

/* { dg-final { scan-assembler-not   "stxvd2x"  } } */
/* { dg-final { scan-assembler-not   "stxvw4x"  } } */
/* { dg-final { scan-assembler-not   "stxsd"    } } */
/* { dg-final { scan-assembler-not   "stxsdx"   } } */
/* { dg-final { scan-assembler-not   "lxvd2x"   } } */
/* { dg-final { scan-assembler-not   "lxvw4x"   } } */
/* { dg-final { scan-assembler-not   "lxsd"     } } */
/* { dg-final { scan-assembler-not   "lxsdx"    } } */
/* { dg-final { scan-assembler-times "ld"     1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "srdi"   1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "lwz"    1 { target ilp32 } } } */
/* { dg-final { scan-assembler-times "rlwinm" 1 { target ilp32 } } } */
