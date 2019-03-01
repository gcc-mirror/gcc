/* Test that using a character splat to set up a shift-right logical
   for a doubleword vector works correctly after gimple folding.  */

/* { dg-do run { target { p8vector_hw } } } */
/* { dg-options "-O2 -mpower8-vector" } */

#include <altivec.h>

typedef __vector unsigned long long vui64_t;

static inline vui64_t
vec_srdi (vui64_t vra, const unsigned int shb)
{
  vui64_t rshift;
  vui64_t result;

  /* Note legitimate use of wrong-type splat due to expectation that only
     lower 6-bits are read.  */
  rshift = (vui64_t) vec_splat_s8 (shb);

  /* Vector Shift Right [Logical] Doublewords based on the lower 6-bits
     of corresponding element of rshift.  */
  result = vec_vsrd (vra, rshift);

  return result;
}

__attribute__ ((noinline)) vui64_t
test_srdi_4 (vui64_t a)
{
  return vec_srdi (a, 4);
}

int
main ()
{
  vui64_t x = {1992357, 1025};
  x = test_srdi_4 (x);
  if (x[0] != 124522 || x[1] != 64)
    __builtin_abort ();
  return 0;
}
