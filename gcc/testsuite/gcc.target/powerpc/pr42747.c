/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power7 -ffast-math" } */

double foo (double x) { return __builtin_sqrt (x); }

/* { dg-final { scan-assembler "xssqrtdp\|fsqrt" } } */
