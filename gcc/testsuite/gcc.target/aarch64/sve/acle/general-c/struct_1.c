/* { dg-options "-std=c99" } */

#include <arm_sve.h>

void
f (svint8_t a, svint8_t b)
{
  /* Not supported, but mustn't ICE.  */
  (svint8x2_t) { a, b };
}
