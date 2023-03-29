// P1467R9 - Extended floating-point types and standard names.
// { dg-do compile { target c++23 } }
// { dg-options "" }

#include "ext-floating.h"

#ifdef __STRICT_ANSI__
#undef __SIZEOF_FLOAT128__
#endif

extern "C" void abort ();

using namespace std;

template <typename T, typename U> 
int
foo (T x, U y) noexcept
{
  return 3;
}

int
main ()
{
  if (foo (0.0f, 0.0f) != 3)
    abort ();
  if (foo (0.0, 0.0) != 3)
    abort ();
  if (foo (0.0L, 0.0L) != 3)
    abort ();
#ifdef __STDCPP_FLOAT16_T__
  if (foo (0.0f16, 0.0f16) != 3)
    abort ();
  if (foo (0.0f, 0.0f16) != 3)
    abort ();
#endif
#ifdef __STDCPP_FLOAT32_T__
  if (foo (0.0f32, 0.0f32) != 3)
    abort ();
  if (foo (0.0f, 0.0f32) != 3)
    abort ();
#endif
#ifdef __STDCPP_FLOAT64_T__
  if (foo (0.0f64, 0.0f64) != 3)
    abort ();
  if (foo (0.0, 0.0f64) != 3)
    abort ();
#endif
#ifdef __STDCPP_FLOAT128_T__
  if (foo (0.0f128, 0.0f128) != 3)
    abort ();
  if (foo (0.0L, 0.0f128) != 3)
    abort ();
#endif
#ifdef __STDCPP_BFLOAT16_T__
  if (foo (0.0bf16, 0.0bf16) != 3)
    abort ();
  if (foo (0.0f, 0.0bf16) != 3)
    abort ();
#endif
#ifdef __FLT32X_MANT_DIG__
  if (foo (0.0f32x, 0.0f32x) != 3)
    abort ();
  if (foo (0.0, 0.0f32x) != 3)
    abort ();
#endif
#ifdef __FLT64X_MANT_DIG__
  if (foo (0.0f64x, 0.0f64x) != 3)
    abort ();
  if (foo (0.0L, 0.0f64x) != 3)
    abort ();
#endif
#ifdef __FLT128X_MANT_DIG__
  if (foo (0.0f128x, 0.0f128x) != 3)
    abort ();
  if (foo (0.0L, 0.0f128x) != 3)
    abort ();
#endif
}
