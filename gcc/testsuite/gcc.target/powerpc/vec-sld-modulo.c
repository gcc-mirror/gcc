/* Test that using a character splat to set up a shift-left
   for a doubleword vector works correctly after gimple folding.  */

/* { dg-do run { target { p8vector_hw } } } */
/* { dg-options "-O2 -mvsx" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */

#include <altivec.h>

typedef __vector unsigned long long vui64_t;

static inline vui64_t
vec_sldi (vui64_t vra, const unsigned int shb)
{
  vui64_t lshift;
  vui64_t result;

  /* Note legitimate use of wrong-type splat due to expectation that only
     lower 6-bits are read.  */
  lshift = (vui64_t) vec_splat_s8 (shb);

  /* Vector Shift Left Doublewords based on the lower 6-bits
     of corresponding element of lshift.  */
  result = vec_vsld (vra, lshift);

  return result;
}

__attribute__ ((noinline)) vui64_t
test_sldi_4 (vui64_t a)
{
  return vec_sldi (a, 4);
}

int
main ()
{
  vui64_t x = {-256, 1025};
  x = test_sldi_4 (x);
  if (x[0] != -4096 || x[1] != 16400)
    __builtin_abort ();
  return 0;
}
