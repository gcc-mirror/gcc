/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer -ffast-math" } */
/* { dg-require-effective-target ilp32 } */

static float __attribute__((noinline)) foo_f(float f) { return f; }
static double __attribute__((noinline)) foo_d(double d) { return d; }
static long double __attribute__((noinline)) foo_ld(long double ld) { return ld; }

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

/* { dg-final { scan-assembler-not "\\(%esp\\)" { xfail *-*-* } } } */
