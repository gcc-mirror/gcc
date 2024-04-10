/* { dg-require-effective-target ppc_float128_hw } */
/* { dg-options "-mvsx -O2 -ffast-math" } */
/* { dg-additional-options "-mdejagnu-cpu=power9" { target { ! has_arch_pwr9 } } } */

#ifndef TYPE
#define TYPE _Float128
#endif

/* Test that the fminf128/fmaxf128 functions generate if/then/else and not a
   call.  */
TYPE f128_min (TYPE a, TYPE b) { return __builtin_fminf128 (a, b); }
TYPE f128_max (TYPE a, TYPE b) { return __builtin_fmaxf128 (a, b); }

/* Adjust code power10 which has native min/max instructions.  */
/* { dg-final { scan-assembler-times {\mxscmpuqp\M} 2 { target { ! has_arch_pwr10 } } } } */
/* { dg-final { scan-assembler-times {\mxsmincqp\M} 1 { target has_arch_pwr10 } } } */
/* { dg-final { scan-assembler-times {\mxsmaxcqp\M} 1 { target has_arch_pwr10 } } } */
/* { dg-final { scan-assembler-not {\mbl\M} } } */
