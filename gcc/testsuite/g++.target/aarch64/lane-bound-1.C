// { dg-do compile }
// { dg-options "" }
#include <arm_neon.h>

// vgetq_lane_u64 should not cause any
// exceptions to thrown so even at -O0
// removeme should have been removed.
void removeme()
__attribute__((error("nothrow")));
int _setjmp();
void hh(uint64x2_t c, int __b)
{
  try {
    vgetq_lane_u64(c, __b);
    // { dg-error "must be a constant immediate" "" { target *-*-* } 0 }
  } catch (...)
  {
        removeme(); // { dg-bogus "declared with attribute error" }
  }
}

