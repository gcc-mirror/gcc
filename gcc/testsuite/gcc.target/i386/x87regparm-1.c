/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer" } */
/* { dg-require-effective-target ilp32 } */

float foo_f(float) __attribute__((x87regparm));
double foo_d(double) __attribute__((x87regparm));
long double foo_ld(long double) __attribute__((x87regparm));

volatile float f;
volatile double d;
volatile long double ld;

void test() 
{
  f = foo_f(f);
  d = foo_d(d);
  ld = foo_ld(ld);
}

/* Check that no memory is used to pass arguments.  */

/* { dg-final { scan-assembler-not "\\(%esp\\)" } } */
