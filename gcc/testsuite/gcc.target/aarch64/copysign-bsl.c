/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Test that we can generate DImode BSL when we are using
   copysign.  */

double
foo (double a, double b)
{
  return __builtin_copysign (a, b);
}

/* { dg-final { scan-assembler "b\(sl|it|if\)\tv\[0-9\]" } } */
