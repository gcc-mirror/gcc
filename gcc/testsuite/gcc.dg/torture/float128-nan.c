/* Test __float128 NaN generation.  */
/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-options "" } */

#include <fenv.h>
#include <stdbool.h>
#include <stdint.h>

typedef union
{
  __float128 value;

  struct
#ifdef __MINGW32__
  /* Make sure we are using gnu-style bitfield handling.  */
  __attribute__ ((gcc_struct))
#endif
  {
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    unsigned negative:1;
    unsigned exponent:15;
    unsigned quiet_nan:1;
    uint64_t mant_high:47;
    uint64_t mant_low:64;
#else
    uint64_t mant_low:64;
    uint64_t mant_high:47;
    unsigned quiet_nan:1;
    unsigned exponent:15;
    unsigned negative:1;
#endif
  } nan;

} ieee854_float128;

bool
__attribute__((noinline, noclone))
check_nan (__float128 val, bool quiet)
{
  ieee854_float128 u;
  volatile __float128 tmp;

  u.value = val;

  if (u.nan.exponent != 0x7fff
      || (u.nan.quiet_nan | u.nan.mant_high | u.nan.mant_low) == 0
      || u.nan.quiet_nan != quiet)
    return false;

  if (!__builtin_isnan (u.value))
    return false;

  feclearexcept (FE_INVALID);

  tmp = u.value + u.value;

  if ((fetestexcept (FE_INVALID) != 0) == quiet)
    return false;

  return true;
}

int
main (void)
{
  __float128 nan;

  nan = __builtin_nanq ("");

  if (!check_nan (nan, true))
    __builtin_abort ();

  nan = __builtin_nanq ("0x0");

  if (!check_nan (nan, true))
    __builtin_abort ();

  nan = __builtin_nanq ("0x1");

  if (!check_nan (nan, true))
    __builtin_abort ();

  nan = __builtin_nansq ("");

  if (!check_nan (nan, false))
    __builtin_abort ();

  nan = __builtin_nansq ("0x0");

  if (!check_nan (nan, false))
    __builtin_abort ();

  nan = __builtin_nansq ("0x1");

  if (!check_nan (nan, false))
    __builtin_abort ();

  return 0;
}
