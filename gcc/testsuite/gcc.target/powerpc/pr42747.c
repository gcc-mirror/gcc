/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O2 -mdejagnu-cpu=power7 -ffast-math" } */
/* { dg-require-effective-target powerpc_vsx } */

double foo (double x) { return __builtin_sqrt (x); }

/* { dg-final { scan-assembler "xssqrtdp\|fsqrt" } } */
