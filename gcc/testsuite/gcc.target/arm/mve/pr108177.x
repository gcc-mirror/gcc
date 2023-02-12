#include "pr108177-main.x"

__attribute__ ((noipa)) void
test (uint32x4_t v, void *a, mve_pred16_t p1, mve_pred16_t p2)
{
  TYPE _v = (TYPE) v;
  INTRINSIC_P (a, _v, p1);
  INTRINSIC_P (a, _v, p2);
}
