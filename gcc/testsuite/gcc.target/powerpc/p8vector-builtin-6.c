/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

vector float dbl_to_float_p8 (double x) { return __builtin_vsx_xscvdpspn (x); }
double float_to_dbl_p8 (vector float x) { return __builtin_vsx_xscvspdpn (x); }

/* { dg-final { scan-assembler "xscvdpspn" } } */
/* { dg-final { scan-assembler "xscvspdpn" } } */
