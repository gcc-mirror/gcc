/* __builtin_fpclassify must not be expanded into FP comparisons either: a
   comparison cannot tell FP_SUBNORMAL from FP_ZERO when the subnormal
   operand is flushed to zero, and traps when it is not.  */

/* { dg-do compile } */
/* { dg-options "-O2" } */

int
fpclassify_f (float x)
{
  return __builtin_fpclassify (0, 1, 4, 3, 2, x);
}

int
fpclassify_d (double x)
{
  return __builtin_fpclassify (0, 1, 4, 3, 2, x);
}

/* { dg-final { scan-assembler-not "\tcmpt" } } */
