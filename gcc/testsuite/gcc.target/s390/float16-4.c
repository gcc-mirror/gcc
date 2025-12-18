/* { dg-do run { target float16 } } */
/* { dg-options "-O2 -fsignaling-nans -save-temps" } */
/* { dg-final { scan-assembler-times "brasl\t%r14,__truncsfhf2@PLT" 1 } } */
/* { dg-final { scan-assembler-times "brasl\t%r14,__truncdfhf2@PLT" 1 } } */
/* { dg-final { scan-assembler-times "brasl\t%r14,__trunctfhf2@PLT" 1 } } */
/* { dg-final { scan-assembler-times "brasl\t%r14,__extendhfsf2@PLT" 1 } } */
/* { dg-final { scan-assembler-times "brasl\t%r14,__extendhfdf2@PLT" 1 } } */
/* { dg-final { scan-assembler-times "brasl\t%r14,__extendhftf2@PLT" 1 } } */

#define _GNU_SOURCE

#include <assert.h>
#include <fenv.h>
#include <math.h>

#pragma STDC FENV_ACCESS ON

#define fn_truncate(mode, type) \
  [[gnu::noipa]] _Float16 \
  truncate##mode##hf (type x) { return x; }

fn_truncate (sf, float)
fn_truncate (df, double)
fn_truncate (tf, long double)

#define fn_extend(mode, type) \
  [[gnu::noipa]] type \
  extendhf##mode (_Float16 x) { return x; }

fn_extend (sf, float)
fn_extend (df, double)
fn_extend (tf, long double)

int
main (void)
{
  feclearexcept (FE_ALL_EXCEPT);

  /* Don't use isnan() but rather check manually since otherwise values of type
     _Float16 would be extended before being passed to isnan() and we really
     want to verify type _Float16 here.  */
#define test_truncate_nan(mode, fn) \
  { \
    unsigned short tmp; \
    _Float16 x; \
    x = truncate##mode##hf (__builtin_nans##fn ("42")); \
    assert (fetestexcept (FE_ALL_EXCEPT) == FE_INVALID); \
    __builtin_memcpy (&tmp, &x, 2); \
    assert (tmp == 0x7E00); \
    feclearexcept (FE_ALL_EXCEPT); \
    x = truncate##mode##hf (__builtin_nan##fn ("42")); \
    assert (fetestexcept (FE_ALL_EXCEPT) == 0); \
    __builtin_memcpy (&tmp, &x, 2); \
    assert (tmp == 0x7E00); \
  }

  test_truncate_nan (sf, f)
  test_truncate_nan (df,  )
  test_truncate_nan (tf, l)

#define test_truncate_inexact_overflow(mode) \
  { \
    truncate##mode##hf (__FLT_MAX__); \
    assert (fetestexcept (FE_ALL_EXCEPT) == (FE_INEXACT | FE_OVERFLOW)); \
    feclearexcept (FE_ALL_EXCEPT); \
    truncate##mode##hf (42.f); \
    assert (fetestexcept (FE_ALL_EXCEPT) == 0); \
  }

  test_truncate_inexact_overflow (sf)
  test_truncate_inexact_overflow (df)
  test_truncate_inexact_overflow (tf)

#define test_truncate_inexact_underflow(mode) \
  { \
    truncate##mode##hf (__FLT_MIN__); \
    assert (fetestexcept (FE_ALL_EXCEPT) == (FE_INEXACT | FE_UNDERFLOW)); \
    feclearexcept (FE_ALL_EXCEPT); \
    truncate##mode##hf (-42.f); \
    assert (fetestexcept (FE_ALL_EXCEPT) == 0); \
  }

  test_truncate_inexact_underflow (sf)
  test_truncate_inexact_underflow (df)
  test_truncate_inexact_underflow (tf)

#define test_extend(type, mode) \
  { \
    type x; \
    x = extendhf##mode (__builtin_nansf16 ("42")); \
    assert (fetestexcept (FE_ALL_EXCEPT) == FE_INVALID); \
    assert (isnan (x)); \
    feclearexcept (FE_ALL_EXCEPT); \
    x = extendhf##mode (__builtin_nanf16 ("42")); \
    assert (fetestexcept (FE_ALL_EXCEPT) == 0); \
    assert (isnan (x)); \
  }

  test_extend (float, sf)
  test_extend (double, df)
  test_extend (long double, tf)

  return 0;
}
