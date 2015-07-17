/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -mno-sse -mno-mmx -mno-80387" } */
/* { dg-final { scan-assembler-not "\.LC\[0-9\]" } } */

double foo (float);

double
f1 (void)
{
  return foo (1.0);
}

double
f2 (void)
{
  return foo (0.0);
}

void
f3 (float *x, float t)
{
  *x = 0.0 + t;
}

float
f4 (void)
{
  return 1.0;
}
