/* Excess precision tests.  Test excess precision is removed when
   necessary.  */
/* { dg-do run } */
/* { dg-options "-O2 -mfpmath=387 -fexcess-precision=standard" } */

#include <float.h>
#include <stdarg.h>

extern void abort (void);
extern void exit (int);

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

void
test_assign (void)
{
  float f;
  double d;
  f = f1 + f2;
  if (f != fadd1)
    abort ();
  d = f1 + f2;
  if (d != dadd2)
    abort ();
  d = d1 + d2 + d3;
  if (d != dadd2)
    abort ();
  /* Verify rounding direct to float without double rounding.  */
  if (sizeof(long double) > sizeof(double)  )  {
    f = d1 + dh + d3;
    if (f != fha)
      abort ();
  } else {
      f = d1 + dh + d3d;
      if (f != fha)
        abort ();
  }
}

void
test_init (void)
{
  float f = f1 + f2;
  double d = d1 + d2 + d3;
  if (f != fadd1)
    abort ();
  if (d != dadd2)
    abort ();
}

volatile int i1 = 0x40000001;
volatile unsigned int u1 = 0x80000001u;
volatile long long ll1 = 0x4000000000000001ll;
volatile unsigned long long ull1 = 0x8000000000000001ull;

void
test_cast (void)
{
  if ((float)(f1 + f2) != fadd1)
    abort ();
  if ((double)(d1 + d2 + d3) != dadd2)
    abort ();
  if ((double)(f1 + f2 + f3) != dadd2)
    abort ();
  if ((float)i1 != 0x1.0p30f)
    abort ();
  if ((float)u1 != 0x1.0p31f)
    abort ();
  if ((float)ll1 != 0x1.0p62f)
    abort ();
  if ((float)ull1 != 0x1.0p63f)
    abort ();
  if ((double)ll1 != 0x1.0p62)
    abort ();
  if ((double)ull1 != 0x1.0p63)
    abort ();
}

static inline void
check_float (float f)
{
  if (f != fadd1)
    abort ();
}

static inline void
check_double (double d)
{
  if (d != dadd2)
    abort ();
}

static inline void
check_float_nonproto (f)
     float f;
{
  if (f != fadd1)
    abort ();
}

static inline void
check_double_nonproto (d)
     double d;
{
  if (d != dadd2)
    abort ();
}

static void
check_double_va (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  if (va_arg (ap, double) != dadd2)
    abort ();
  va_end (ap);
}

void
test_call (void)
{
  check_float (f1 + f2);
  check_double (d1 + d2 + d3);
  check_double (f1 + f2 + f3);
  check_float_nonproto (f1 + f2);
  check_double_nonproto (d1 + d2 + d3);
  check_double_nonproto (f1 + f2 + f3);
  check_double_va (0, d1 + d2 + d3);
  check_double_va (0, f1 + f2 + f3);
}

static inline float
return_float (void)
{
  return f1 + f2;
}

static inline double
return_double1 (void)
{
  return d1 + d2 + d3;
}

static inline double
return_double2 (void)
{
  return f1 + f2 + f3;
}

void
test_return (void)
{
  if (return_float () != fadd1)
    abort ();
  if (return_double1 () != dadd2)
    abort ();
  if (return_double2 () != dadd2)
    abort ();
}

volatile float flt_min = FLT_MIN;
volatile double dbl_min = DBL_MIN;
volatile float flt_max = FLT_MAX;
volatile double dbl_max = DBL_MAX;

void
test_builtin (void)
{
  /* Classification macros convert to the semantic type.  signbit and
     comparison macros do not.  */
  if (!__builtin_isinf (flt_max * flt_max))
    abort ();
  if (!__builtin_isinf (dbl_max * dbl_max))
    abort ();
  if (__builtin_isnormal (flt_max * flt_max))
    abort ();
  if (__builtin_isnormal (dbl_max * dbl_max))
    abort ();
  if (__builtin_isfinite (flt_max * flt_max))
    abort ();
  if (__builtin_isfinite (dbl_max * dbl_max))
    abort ();
  if (!__builtin_isgreater (flt_min * flt_min, 0.0f))
    abort ();
  if (!__builtin_isgreaterequal (flt_min * flt_min, 0.0f))
    abort ();
  if (!__builtin_isless (0.0f, flt_min * flt_min))
    abort ();
  if (__builtin_islessequal (flt_min * flt_min, 0.0f))
    abort ();
  if (!__builtin_islessgreater (flt_min * flt_min, 0.0f))
    abort ();
  if (!__builtin_isgreaterequal (dbl_min * dbl_min, 0.0))
    abort ();
  if (sizeof(long double) > sizeof(double)  ) {
    if (!__builtin_isgreater (dbl_min * dbl_min, 0.0))
      abort ();
    if (!__builtin_isless (0.0, dbl_min * dbl_min))
      abort ();
    if (__builtin_islessequal (dbl_min * dbl_min, 0.0))
      abort ();
    if (!__builtin_islessgreater (dbl_min * dbl_min, 0.0))
      abort ();
  }
  else {
    if (__builtin_isgreater (dbl_min * dbl_min, 0.0))
      abort ();
    if (__builtin_isless (0.0, dbl_min * dbl_min))
      abort ();
    if (!__builtin_islessequal (dbl_min * dbl_min, 0.0))
      abort ();
    if (__builtin_islessgreater (dbl_min * dbl_min, 0.0))
      abort ();
  }
}

int
main (void)
{
  test_assign ();
  test_init ();
  test_cast ();
  test_call ();
  test_return ();
  test_builtin ();
  exit (0);
}
