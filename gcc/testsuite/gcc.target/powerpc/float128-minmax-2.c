/* { dg-require-effective-target ppc_float128_hw } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -ffast-math" } */

#ifndef TYPE
#define TYPE _Float128
#endif

/* Test that the fminf128/fmaxf128 functions generate if/then/else and not a
   call.  */
TYPE f128_min (TYPE a, TYPE b) { return __builtin_fminf128 (a, b); }
TYPE f128_max (TYPE a, TYPE b) { return __builtin_fmaxf128 (a, b); }

/* { dg-final { scan-assembler {\mxsmaxcqp\M} } } */
/* { dg-final { scan-assembler {\mxsmincqp\M} } } */
