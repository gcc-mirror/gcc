/* Excess precision tests.  Test that excess precision is carried
   through various operations.  */
/* { dg-do run } */
/* { dg-options "-O2 -mfpmath=387 -fexcess-precision=standard" } */

#include <float.h>

extern void abort (void);
extern void exit (int);

volatile float f1 = 1.0f;
volatile float f2 = 0x1.0p-30f;
volatile float f3 = 0x1.0p-60f;
volatile double d1 = 1.0;
volatile double d2 = 0x1.0p-30;
volatile double d3 = 0x1.0p-60;
volatile float fadd1 = 1.0f + 0x1.0p-30f;
volatile double dadd2 = 1.0 + 0x1.0p-30 + 0x1.0p-60;
volatile long double ldadd1 = 1.0l + 0x1.0p-30l;
volatile long double ldadd2 = 1.0l + 0x1.0p-30l + 0x1.0p-60l;

void
test_add (void)
{
  if (f1 + f2 != ldadd1)
    abort ();
  if (f1 + f2 + f3 != ldadd2)
    abort ();
  if (d1 + d2 != ldadd1)
    abort ();
  if (d1 + d2 + d3 != ldadd2)
    abort ();
  if (f1 + d2 + f3 != ldadd2)
    abort ();
  if (f1 + f2 == fadd1)
    abort ();
  if (f1 + f2 <= fadd1)
    abort ();
  if (f1 + f2 < fadd1)
    abort ();
  if (sizeof(long double) > sizeof(double)) {
    if ( d1 + d2 + d3 == dadd2)
      abort ();
    if (!(d1 + d2 + d3 > dadd2))
      abort ();
    if (!(d1 + d2 + d3 >= dadd2))
      abort ();
  }
  else {
    if ( d1 + d2 + d3 != dadd2 )
      abort();
    if ( d1 + d2 + d3 < dadd2 )
      abort();
    if ( d1 + d2 + d3 > dadd2 )
      abort();
  }
}

volatile long double ldsub1 = 1.0l - 0x1.0p-30l;
volatile long double ldsub2 = 1.0l - 0x1.0p-30l - 0x1.0p-60l;

void
test_sub (void)
{
  if (f1 - f2 != ldsub1)
    abort ();
  if (f1 - f2 - f3 != ldsub2)
    abort ();
  if (d1 - d2 != ldsub1)
    abort ();
  if (d1 - d2 - d3 != ldsub2)
    abort ();
  if (f1 - d2 - f3 != ldsub2)
    abort ();
  if (+(f1 - d2 - f3) != ldsub2)
    abort ();
  if (-(f1 - d2 - f3) != -ldsub2)
    abort ();
}

volatile float flt_min = FLT_MIN;
volatile double dbl_min = DBL_MIN;
volatile long double flt_min2 = (long double)FLT_MIN * (long double)FLT_MIN;
volatile long double dbl_min3 = (long double)DBL_MIN * (long double)DBL_MIN * (long double)DBL_MIN;

void
test_mul (void)
{
  if (flt_min * flt_min != flt_min2)
    abort ();
  if (flt_min * flt_min == 0)
    abort ();
  if (flt_min * flt_min == 0)
    abort ();
  if (!(flt_min * flt_min))
    abort ();
  if (dbl_min * dbl_min * dbl_min != dbl_min3)
    abort ();
  if ((long double)(dbl_min * dbl_min * dbl_min) != dbl_min3)
    abort ();
  if ((0, dbl_min * dbl_min * dbl_min) != dbl_min3)
    abort ();
  if (sizeof(long double) > sizeof(double) ) {
    if (dbl_min * dbl_min * dbl_min == 0)
      abort ();
    if ((flt_min * flt_min ? dbl_min * dbl_min * dbl_min : 0) == 0)
      abort ();
  }
  else {
    if (dbl_min * dbl_min * dbl_min != 0)
      abort ();
    if ((flt_min * flt_min ? dbl_min * dbl_min * dbl_min : 1) != 0)
      abort ();
  }
  if ((flt_min * flt_min ? : 0) == 0)
    abort ();
}

volatile float f4 = 0x1.0p100f;
volatile double d4 = 0x1.0p100;
volatile long double flt_div = 0x1.0p100l / (long double) FLT_MIN;
volatile long double dbl_div = 0x1.0p100l / (long double) DBL_MIN;

void
test_div (void)
{
  if (f4 / flt_min != flt_div)
    abort ();
  if (d4 / dbl_min != dbl_div)
    abort ();
}

volatile float f5 = 0x1.0p30;

void
test_cast (void)
{
  if ((int)(f1 + f5) != 0x40000001)
    abort ();
}

volatile float _Complex f1c = 1.0f + 1.0if;
volatile float _Complex f2c = 0x1.0p-30f + 0x1.0p-31if;
volatile float _Complex f3c = 0x1.0p-60f + 0x1.0p-59if;
volatile double _Complex d1c = 1.0 + 1.0i;
volatile double _Complex d2c = 0x1.0p-30 + 0x1.0p-31i;
volatile double _Complex d3c = 0x1.0p-60 + 0x1.0p-59i;
volatile long double _Complex ldadd1c = 1.0l + 0x1.0p-30l + 1.0il + 0x1.0p-31il;
volatile long double _Complex ldadd2c = 1.0l + 0x1.0p-30l + 0x1.0p-60l + 1.0il + 0x1.0p-31il + 0x1.0p-59il;
volatile long double _Complex ldadd2cc = 1.0l + 0x1.0p-30l + 0x1.0p-60l - 1.0il - 0x1.0p-31il - 0x1.0p-59il;
volatile float _Complex flt_minc = FLT_MIN;
volatile double _Complex dbl_minc = DBL_MIN;
volatile float _Complex f4c = 0x1.0p100f;
volatile double _Complex d4c = 0x1.0p100;

void
test_complex (void)
{
  if (f1c + f2c != ldadd1c)
    abort ();
  if (f1c + f2c + f3c != ldadd2c)
    abort ();
  if (d1c + d2c != ldadd1c)
    abort ();
  if (d1c + d2c + d3c != ldadd2c)
    abort ();
  if (__real__ (f1c + f2c + f3c) != ldadd2)
    abort ();
  if (__imag__ (d1c + d2c + d3c) != __imag__ ldadd2c)
    abort ();
  if (~(d1c + d2c + d3c) != ldadd2cc)
    abort ();
  /* The following call libgcc functions and so would fail unless they
     call those for long double.  */
  if (flt_minc * flt_minc != flt_min2)
    abort ();
  if (dbl_minc * dbl_minc * dbl_minc != dbl_min3)
    abort ();
  if (f4c / flt_minc != flt_div)
    abort ();
  if (d4c / dbl_minc != dbl_div)
    abort ();
  if (f4 / flt_minc != flt_div)
    abort ();
  if (d4 / dbl_minc != dbl_div)
    abort ();
  if (f4c / flt_min != flt_div)
    abort ();
  if (d4c / dbl_min != dbl_div)
    abort ();
}

int
main (void)
{
  test_add ();
  test_sub ();
  test_mul ();
  test_div ();
  test_cast ();
  test_complex ();
  exit (0);
}
