/* { dg-do run { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target ppc_float128_sw } */
/* { dg-options "-mcpu=power7 -O2 -mfloat128 -lm" } */

#ifdef DEBUG
#include <stdio.h>
#endif

#include <stddef.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdlib.h>
#include <math.h>

#if defined(__BIG_ENDIAN__)
struct ieee128 {
  uint64_t upper;
  uint64_t lower;
};

#elif defined(__LITTLE_ENDIAN__)
struct ieee128 {
  uint64_t lower;
  uint64_t upper;
};

#else
#error "Unknown system"
#endif

union ieee_union {
  __float128 f128;
  struct ieee128 st128;
};

#ifdef DEBUG
static int num_errors = 0;

__attribute__((__noinline__))
static void
failure (int expected, int got, __float128 x)
{
  unsigned sign;
  unsigned exponent;
  uint64_t mantissa1;
  uint64_t mantissa2;
  uint64_t upper;
  uint64_t lower;

  union ieee_union u;

  u.f128 = x;
  upper  = u.st128.upper;
  lower  = u.st128.lower;

  sign      = (unsigned)((upper >> 63) & 1);
  exponent  = (unsigned)((upper >> 48) & ((((uint64_t)1) << 16) - 1));
  mantissa1 = (upper & ((((uint64_t)1) << 48) - 1));
  mantissa2 = lower;

  printf ("Expected %d, got %d, %c 0x%.4x 0x%.12" PRIx64 " 0x%.16" PRIx64,
	  expected, got,
	  sign ? '-' : '+',
	  exponent,
	  mantissa1,
	  mantissa2);

  num_errors++;
}

#else

#define failure(E, G, F) abort ()
#endif

__attribute__((__noinline__))
static void
test_signbit_arg (__float128 f128, int expected)
{
  int sign = __builtin_signbit (f128);

  if ((expected != 0 && sign == 0)
      || (expected == 0 && sign != 0))
    failure (f128, expected, sign);
}

__attribute__((__noinline__))
static void
test_signbit_mem (__float128 *ptr, int expected)
{
  int sign = __builtin_signbit (*ptr);

  if ((expected != 0 && sign == 0)
      || (expected == 0 && sign != 0))
    failure (*ptr, expected, sign);
}

__attribute__((__noinline__))
static void
test_signbit_gpr (__float128 *ptr, int expected)
{
  __float128 f128 = *ptr;
  int sign;

  __asm__ (" # %0" : "+r" (f128));

  sign = __builtin_signbit (f128);
  if ((expected != 0 && sign == 0)
      || (expected == 0 && sign != 0))
    failure (f128, expected, sign);
}

__attribute__((__noinline__))
static void
test_signbit (__float128 f128, int expected)
{
#ifdef DEBUG
  union ieee_union u;
  u.f128 = f128;
  printf ("Expecting %d, trying %-5g "
	  "(0x%.16" PRIx64 " 0x%.16" PRIx64 ")\n",
	  expected, (double)f128,
	  u.st128.upper, u.st128.lower);
#endif

  test_signbit_arg (f128,  expected);
  test_signbit_mem (&f128, expected);
  test_signbit_gpr (&f128, expected);
}

int
main (void)
{
  union ieee_union u;

  test_signbit (+0.0q, 0);
  test_signbit (+1.0q, 0);

  test_signbit (-0.0q, 1);
  test_signbit (-1.0q, 1);

  test_signbit (__builtin_copysign (__builtin_infq (), +1.0q), 0);
  test_signbit (__builtin_copysign (__builtin_infq (), -1.0q), 1);

  test_signbit (__builtin_copysign (__builtin_nanq (""), +1.0q), 0);
  test_signbit (__builtin_copysign (__builtin_nanq (""), -1.0q), 1);

  /* force the bottom double word to have specific bits in the 'sign' bit to
     make sure we are picking the right word.  */
  u.f128 = 1.0q;
  u.st128.lower = 0ULL;
  test_signbit (u.f128, 0);

  u.st128.lower = ~0ULL;
  test_signbit (u.f128, 0);

  u.f128 = -1.0q;
  u.st128.lower = 0ULL;
  test_signbit (u.f128, 1);

  u.st128.lower = ~0ULL;
  test_signbit (u.f128, 1);

#ifdef DEBUG
  printf ("%d error(s) were found\n", num_errors);
  if (num_errors)
    return num_errors;
#endif

  return 0;
}

