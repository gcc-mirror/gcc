/* Test that using a character splat to set up a shift-right algebraic
   for a doubleword vector works correctly after gimple folding.  */

/* { dg-do run { target { p8vector_hw } } } */
/* { dg-options "-O2 -mpower8-vector" } */

#include <altivec.h>

typedef __vector unsigned long long vui64_t;
typedef __vector long long vi64_t;

static inline vi64_t
vec_sradi (vi64_t vra, const unsigned int shb)
{
  vui64_t rshift;
  vi64_t result;

  /* Note legitimate use of wrong-type splat due to expectation that only
     lower 6-bits are read.  */
  rshift = (vui64_t) vec_splat_s8 (shb);

  /* Vector Shift Right Algebraic Doublewords based on the lower 6-bits
     of corresponding element of rshift.  */
  result = vec_vsrad (vra, rshift);

  return result;
}

__attribute__ ((noinline)) vi64_t
test_sradi_4 (vi64_t a)
{
  return vec_sradi (a, 4);
}

int
main ()
{
  vi64_t x = {-256, 1025};
  x = test_sradi_4 (x);
  if (x[0] != -16 || x[1] != 64)
    __builtin_abort ();
  return 0;
}
