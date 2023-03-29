// Excess precision tests.  Test excess precision is removed when
// necessary.
// { dg-do run }
// { dg-options "-O2 -mfpmath=387 -fexcess-precision=standard" }

#include <float.h>
#include <stdarg.h>

extern "C" void abort ();

volatile float f1 = 1.0f;
volatile float f2 = 0x1.0p-30f;
volatile float f3 = 0x1.0p-60f;
volatile double d1 = 1.0;
volatile double d2 = 0x1.0p-30;
volatile double d3 = 0x1.0p-60;
volatile double d3d = 0x1.0p-52;
volatile float fadd1 = 1.0f + 0x1.0p-30f;
volatile double dadd2 = 1.0 + 0x1.0p-30 + 0x1.0p-60;
volatile double dh = 0x1.0p-24;
volatile float fha = 1.0f + 0x1.0p-23f;

static inline void
check_float (float f)
{
  if (f != fadd1)
    abort ();
}

static inline void
check_float (double)
{
  abort ();
}

static inline void
check_float (long double)
{
  abort ();
}

static inline void
check_double (double d)
{
  if (d != dadd2)
    abort ();
}

static inline void
check_double (long double)
{
  abort ();
}

static inline void
check_float2 (float f)
{
  if (f != fha)
    abort ();
}

struct S {
  S () {}
  S (float f) { if (f != fadd1) abort (); }
};

struct T {
  T () {}
  T (double d) { if (d != dadd2) abort (); }
};

static inline void
check_float3 (S)
{
}

static inline void
check_double2 (T)
{
}

void
test_call ()
{
  check_float (f1 + f2);
  check_double (f1 + f2);
  check_double (d1 + d2 + d3);
  /* Verify rounding direct to float without double rounding.  */
  if (sizeof (long double) > sizeof (double))
    check_float2 (d1 + dh + d3);
  else
    check_float2 (d1 + dh + d3d);
  check_float3 (f1 + f2);
  check_double2 (f1 + f2);
  check_double2 (d1 + d2 + d3);
  S s1 = static_cast<S> (f1 + f2);
  T t2 = static_cast<T> (f1 + f2);
  T t3 = static_cast<T> (d1 + d2 + d3);
}

int
main ()
{
  test_call ();
}
