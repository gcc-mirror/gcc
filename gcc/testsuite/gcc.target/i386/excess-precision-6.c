/* Excess precision tests.  Make sure sqrt is not inlined for float or
   double.  */
/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -fno-math-errno -fexcess-precision=standard" } */

float f;
double d;

float fr;
double dr;

void
test_builtins (void)
{
  fr = __builtin_sqrtf (f);
  dr = __builtin_sqrt (d);
}

/* { dg-final { scan-assembler-not "fsqrt" } } */
