/* { dg-do run { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target ppc_float128_sw } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-mvsx -O2" } */

#ifdef DEBUG
#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include <inttypes.h>
#endif

#if !defined(__FLOAT128__) || !defined(_ARCH_PPC)
static __float128
pass_through (__float128 x)
{
  return x;
}

__float128 (*no_optimize) (__float128) = pass_through;
#endif

#ifdef DEBUG
__attribute__((__noinline__))
static void
print_f128 (__float128 x)
{
  unsigned sign;
  unsigned exponent;
  uint64_t mantissa1;
  uint64_t mantissa2;
  uint64_t upper;
  uint64_t lower;

#if defined(_ARCH_PPC) && defined(__BIG_ENDIAN__)
  struct ieee128 {
    uint64_t upper;
    uint64_t lower;
  };

#elif (defined(_ARCH_PPC) && defined(__LITTLE_ENDIAN__)) || defined(__x86_64__)
  struct ieee128 {
    uint64_t lower;
    uint64_t upper;
  };

#else
#error "Unknown system"
#endif

  union {
    __float128 f128;
    struct ieee128 s128;
  } u;

  u.f128 = x;
  upper  = u.s128.upper;
  lower  = u.s128.lower;

  sign      = (unsigned)((upper >> 63) & 1);
  exponent  = (unsigned)((upper >> 48) & ((((uint64_t)1) << 16) - 1));
  mantissa1 = (upper & ((((uint64_t)1) << 48) - 1));
  mantissa2 = lower;

  printf ("%c 0x%.4x 0x%.12" PRIx64 " 0x%.16" PRIx64,
	  sign ? '-' : '+',
	  exponent,
	  mantissa1,
	  mantissa2);
}
#endif

__attribute__((__noinline__))
static void
do_test (__float128 expected, __float128 got, const char *name)
{
  int equal_p = (expected == got);

#ifdef DEBUG
  printf ("Test %s, expected: ", name);
  print_f128 (expected);
  printf (" %5g, got: ", (double) expected);
  print_f128 (got);
  printf (" %5g, result %s\n",
	  (double) got,
	  (equal_p) ? "equal" : "not equal");
#endif

  if (!equal_p)
    __builtin_abort ();
}


int
main (void)
{
  __float128 one	= 1.0q;
  __float128 two	= 2.0q;
  __float128 three	= 3.0q;
  __float128 four	= 4.0q;
  __float128 five	= 5.0q;
  __float128 add_result = (1.0q + 2.0q);
  __float128 mul_result = ((1.0q + 2.0q) * 3.0q);
  __float128 div_result = (((1.0q + 2.0q) * 3.0q) / 4.0q);
  __float128 sub_result = ((((1.0q + 2.0q) * 3.0q) / 4.0q) - 5.0q);
  __float128 neg_result = - sub_result;
  __float128 add_xresult;
  __float128 mul_xresult;
  __float128 div_xresult;
  __float128 sub_xresult;
  __float128 neg_xresult;

#if defined(__FLOAT128__) && defined(_ARCH_PPC)
  __asm__ (" #prevent constant folding, %x0" : "+wa" (one));
  __asm__ (" #prevent constant folding, %x0" : "+wa" (two));
  __asm__ (" #prevent constant folding, %x0" : "+wa" (three));
  __asm__ (" #prevent constant folding, %x0" : "+wa" (four));
  __asm__ (" #prevent constant folding, %x0" : "+wa" (five));

#else
  one   = no_optimize (one);
  two   = no_optimize (two);
  three = no_optimize (three);
  four  = no_optimize (four);
  five  = no_optimize (five);
#endif

  add_xresult = (one + two);
  do_test (add_result, add_xresult, "add");

  mul_xresult = add_xresult * three;
  do_test (mul_result, mul_xresult, "mul");

  div_xresult = mul_xresult / four;
  do_test (div_result, div_xresult, "div");

  sub_xresult = div_xresult - five;
  do_test (sub_result, sub_xresult, "sub");

  neg_xresult = - sub_xresult;
  do_test (neg_result, neg_xresult, "neg");

#ifdef DEBUG
  printf ("Passed\n");
#endif

  return 0;
}
