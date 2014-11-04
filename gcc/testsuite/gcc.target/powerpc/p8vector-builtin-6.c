/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O2" } */

vector float dbl_to_float_p8 (double x) { return __builtin_vsx_xscvdpspn (x); }
double float_to_dbl_p8 (vector float x) { return __builtin_vsx_xscvspdpn (x); }

/* { dg-final { scan-assembler "xscvdpspn" } } */
/* { dg-final { scan-assembler "xscvspdpn" } } */
