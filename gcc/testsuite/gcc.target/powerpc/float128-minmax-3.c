/* { dg-require-effective-target ppc_float128_hw } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

#ifndef TYPE
#define TYPE _Float128
#endif

/* Test that the fminf128/fmaxf128 functions generate if/then/else and not a
   call.  */
TYPE f128_min (TYPE a, TYPE b) { return (a < b) ? a : b; }
TYPE f128_max (TYPE a, TYPE b) { return (b > a) ? b : a; }

/* { dg-final { scan-assembler {\mxsmaxcqp\M} } } */
/* { dg-final { scan-assembler {\mxsmincqp\M} } } */
