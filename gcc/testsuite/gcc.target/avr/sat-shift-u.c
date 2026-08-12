/* { dg-do link } */
/* { dg-additional-options "-std=gnu99" } */

#include "fx.h"

#define MK_FUN(fx)					\
  fx##_t sat_shift_##fx (sat_##fx##_t a, uint8_t x)	\
  {							\
    return a << x;					\
  }

MK_FUN (uhk)
MK_FUN (uk)
MK_FUN (ulk)
MK_FUN (ullk)

MK_FUN (uhr)
MK_FUN (ur)
MK_FUN (ulr)
MK_FUN (ullr)

int main (void)
{
  return 0;
}
