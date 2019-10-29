#include <arm_sve.h>

void
f (svint8x2_t *a, svint8x2_t *b)
{
  svint8_t *ptr;
  svint8x2_t x = *a;
  *a = *b;
  a = &x;
  (void) (a == b);
  (void) (a != b);
  (void) (a < b);
  (void) (a > b);
  (void) (a <= b);
  (void) (a >= b);
}
