/*
 * Require float128 support because __ibm128 currently is not enabled unless we
 * also have __float128 support.  We require software IEEE 128-bit support,
 * which will work on power8.  If we have hardware IEEE 128-bit support (power9
 * or power10), ppc_float128_sw will still enable running the test.
 */

/* { dg-do run } */
/* { dg-require-effective-target ppc_float128_sw } */
/* { dg-options "-O2 -mvsx -mfloat128" } */
/* { dg-prune-output ".-mfloat128. option may not be fully supported" } */

/*
 * PR target/104253
 *
 * Verify that the various conversions to and from __ibm128 work.  When the
 *  default for long double is changed to IEEE 128-bit, originally GCC would
 *  call the functions using an 'if' name instead of 'tf' name.
 */

#include <stdlib.h>

extern float              ibm128_to_sf  (__ibm128) __attribute__((noinline));
extern double             ibm128_to_df  (__ibm128) __attribute__((noinline));
extern int                ibm128_to_si  (__ibm128) __attribute__((noinline));
extern long long          ibm128_to_di  (__ibm128) __attribute__((noinline));
extern unsigned int       ibm128_to_usi (__ibm128) __attribute__((noinline));
extern unsigned long long ibm128_to_udi (__ibm128) __attribute__((noinline));

extern __ibm128 sf_to_ibm128  (float)              __attribute__((noinline));
extern __ibm128 df_to_ibm128  (double)             __attribute__((noinline));
extern __ibm128 si_to_ibm128  (int)                __attribute__((noinline));
extern __ibm128 di_to_ibm128  (long long)          __attribute__((noinline));
extern __ibm128 usi_to_ibm128 (unsigned int)       __attribute__((noinline));
extern __ibm128 udi_to_ibm128 (unsigned long long) __attribute__((noinline));

float
ibm128_to_sf  (__ibm128 x)
{
  return x;
}

double
ibm128_to_df  (__ibm128 x)
{
  return x;
}

int
ibm128_to_si  (__ibm128 x)
{
  return x;
}

long long
ibm128_to_di  (__ibm128 x)
{
  return x;
}

unsigned int
ibm128_to_usi (__ibm128 x)
{
  return x;
}

unsigned long long
ibm128_to_udi (__ibm128 x)
{
  return x;
}

__ibm128
sf_to_ibm128  (float x)
{
  return x;
}

__ibm128
df_to_ibm128  (double x)
{
  return x;
}

__ibm128
si_to_ibm128  (int x)
{
  return x;
}

__ibm128
di_to_ibm128  (long long x)
{
  return x;
}

__ibm128
usi_to_ibm128 (unsigned int x)
{
  return x;
}

__ibm128
udi_to_ibm128 (unsigned long long x)
{
  return x;
}

volatile float			seven_sf	= 7.0f;
volatile double			seven_df	= 7.0;
volatile int			seven_si	= 7;
volatile long long		seven_di	= 7LL;
volatile unsigned int		seven_usi	= 7U;
volatile unsigned long long	seven_udi	= 7ULL;
volatile __ibm128		seven_ibm128	= 7.0;

int
main (void)
{
  if (seven_ibm128 != sf_to_ibm128 (seven_sf))
    abort ();

  if (seven_ibm128 != df_to_ibm128 (seven_df))
    abort ();

  if (seven_ibm128 != si_to_ibm128 (seven_si))
    abort ();

  if (seven_ibm128 != di_to_ibm128 (seven_di))
    abort ();

  if (seven_ibm128 != usi_to_ibm128 (seven_usi))
    abort ();

  if (seven_ibm128 != udi_to_ibm128 (seven_udi))
    abort ();

  if (seven_sf != ibm128_to_sf (seven_ibm128))
    abort ();

  if (seven_df != ibm128_to_df (seven_ibm128))
    abort ();

  if (seven_si != ibm128_to_si (seven_ibm128))
    abort ();

  if (seven_di != ibm128_to_di (seven_ibm128))
    abort ();

  if (seven_usi != ibm128_to_usi (seven_ibm128))
    abort ();

  if (seven_udi != ibm128_to_udi (seven_ibm128))
    abort ();

  return 0;
}
