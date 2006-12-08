/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer" } */
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

/* Check that float and double arguments are passed through memory.  */

/* { dg-final { scan-assembler-times "\\(%esp\\)" 4 { xfail *-*-* } } } */
