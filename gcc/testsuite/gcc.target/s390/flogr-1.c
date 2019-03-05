/* { dg-do run } */
/* { dg-options "-O2 -funroll-loops -march=z9-109" } */
/* { dg-require-effective-target stdint_types } */

/* Folding of the FLOGR caused a wrong value to be returned by
   __builtin_clz becuase of a problem in the RTX we emit for FLOGR.
   The problematic folding can only be triggered with constants inputs
   introduced on RTL level.  In this case it happens with loop
   unrolling.  */

#include <stdint.h>
#include <assert.h>

static inline uint32_t pow2_ceil_u32(uint32_t x) {
  if (x <= 1) {
    return x;
  }
  int msb_on_index;
  msb_on_index = (31 ^ __builtin_clz(x - 1));
  assert(msb_on_index < 31);
  return 1U << (msb_on_index + 1);
}

void __attribute__((noinline,noclone))
die (int a)
{
  if (a)
    __builtin_abort ();
}

void test_pow2_ceil_u32(void) {
  unsigned i;

  for (i = 0; i < 18; i++) {
      uint32_t a_ = (pow2_ceil_u32(((uint32_t)1) << i));
      if (!(a_ == (((uint32_t)1) << i))) {
	die(1);
      }
  }
}

int
main(void) {
  test_pow2_ceil_u32();

  return 0;
}
