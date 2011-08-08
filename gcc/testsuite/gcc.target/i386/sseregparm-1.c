/* { dg-do compile } */
/* { dg-options "-O2 -msse" } */
/* { dg-require-effective-target ia32 } */

float essef(float) __attribute__((sseregparm));
double essed(double) __attribute__((sseregparm));
float __attribute__((sseregparm, noinline)) ssef(float f) { return f; }
double __attribute__((sseregparm, noinline)) ssed(double d) { return d; }
extern double d;
extern float f;
void test(void)
{
  f = essef(f);
  d = essed(d);
  f = ssef(f);
  d = ssed(d);
}

/* { dg-final { scan-assembler-not "fldl" } } */
