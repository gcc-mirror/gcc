/* This is a small test case for returning a complex number. Written by
   Andreas Jaeger.  */

#include "avx512fp16-xmm-check.h"
#include "defines.h"

#define BUILD_F16_COMPLEX(real, imag) \
  ({ __complex__ _Float16 __retval; \
     __real__ __retval = (real); \
     __imag__ __retval = (imag); \
     __retval; })

__complex__ _Float16
aj_f16_times2 (__complex__ _Float16 x)
{
  __complex__ _Float16 res;

  __real__ res = (2.0 * __real__ x);
  __imag__ res = (2.0 * __imag__ x);

  return res;
}

#define BUILD_F_COMPLEX(real, imag) \
  ({ __complex__ float __retval; \
     __real__ __retval = (real); \
     __imag__ __retval = (imag); \
     __retval; })

#define BUILD_D_COMPLEX(real, imag) \
  ({ __complex__ double __retval; \
     __real__ __retval = (real); \
     __imag__ __retval = (imag); \
     __retval; })

#define BUILD_LD_COMPLEX(real, imag) \
  ({ __complex__ long double __retval; \
     __real__ __retval = (real); \
     __imag__ __retval = (imag); \
     __retval; })

__complex__ float
aj_f_times2 (__complex__ float x)
{
  __complex__ float res;

  __real__ res = (2.0 * __real__ x);
  __imag__ res = (2.0 * __imag__ x);

  return res;
}

__complex__ double
aj_d_times2 (__complex__ double x)
{
  __complex__ double res;

  __real__ res = (2.0 * __real__ x);
  __imag__ res = (2.0 * __imag__ x);

  return res;
}

__complex__ long double
aj_ld_times2 (__complex__ long double x)
{
  __complex__ long double res;

  __real__ res = (2.0 * __real__ x);
  __imag__ res = (2.0 * __imag__ x);

  return res;
}

static void
do_test (void)
{
#ifdef CHECK_COMPLEX
  _Complex _Float16 f16c, f16d;
  _Complex float fc, fd;
  _Complex double dc, dd;
  _Complex long double ldc, ldd;

  f16c = BUILD_F16_COMPLEX (2.0, 3.0);
  f16d = aj_f16_times2 (f16c);

  assert (__real__ f16d == 4.0f16 && __imag__ f16d == 6.0f16);

  fc = BUILD_LD_COMPLEX (2.0f, 3.0f);
  fd = aj_f_times2 (fc);

  assert (__real__ fd == 4.0f && __imag__ fd == 6.0f);

  dc = BUILD_LD_COMPLEX (2.0, 3.0);
  dd = aj_ld_times2 (dc);

  assert (__real__ dd == 4.0 && __imag__ dd == 6.0);

  ldc = BUILD_LD_COMPLEX (2.0L, 3.0L);
  ldd = aj_ld_times2 (ldc);

  assert (__real__ ldd == 4.0L && __imag__ ldd == 6.0L);
#endif
}
