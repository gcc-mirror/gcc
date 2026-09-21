/* The floating-point classification built-ins must not be expanded into FP
   comparisons: without software completion (-mieee) a comparison against a
   subnormal operand either traps or has the operand flushed to zero.  */

/* { dg-do compile } */
/* { dg-options "-O2" } */

int isnan_f (float x) { return __builtin_isnan (x); }
int isnan_d (double x) { return __builtin_isnan (x); }
int isinf_f (float x) { return __builtin_isinf (x); }
int isinf_d (double x) { return __builtin_isinf (x); }
int isfinite_f (float x) { return __builtin_isfinite (x); }
int isfinite_d (double x) { return __builtin_isfinite (x); }
int isnormal_f (float x) { return __builtin_isnormal (x); }
int isnormal_d (double x) { return __builtin_isnormal (x); }

/* { dg-final { scan-assembler-not "\tcmpt" } } */
