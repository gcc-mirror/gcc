/* { dg-do link } */
/* { dg-additional-options "-std=gnu99" } */

#include "fx.h"

#define MK_FUN(fx)					\
  fx##_t sat_shift_##fx (sat_##fx##_t a, uint8_t x)	\
  {							\
    return a << x;					\
  }

MK_FUN (hk)
MK_FUN (k)
MK_FUN (lk)
MK_FUN (llk)

MK_FUN (hr)
MK_FUN (r)
MK_FUN (lr)
MK_FUN (llr)

int main (void)
{
  return 0;
}
