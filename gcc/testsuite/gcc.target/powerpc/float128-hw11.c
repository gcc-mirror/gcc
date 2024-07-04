/* { dg-do run { target lp64 } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-require-effective-target float128 } */
/* { dg-options "-mvsx -O2" } */
/* { dg-additional-options "-mdejagnu-cpu=power9" { target { ! has_arch_pwr9 } } } */

#define __STDC_WANT_IEC_60559_TYPES_EXT__ 1
#define __STDC_WANT_IEC_60559_FUNCS_EXT__ 1

#include <math.h>
#include <stdlib.h>
#include <stddef.h>

extern _Float128 roundf128 (_Float128);
extern _Float128 floorf128 (_Float128);
extern _Float128 ceilf128  (_Float128);
extern _Float128 truncf128 (_Float128);

static const struct {
  _Float128 value;
  _Float128 exp_round;
  _Float128 exp_floor;
  _Float128 exp_ceil;
  _Float128 exp_trunc;
} a[] = {
  { -2.0Q, -2.0Q, -2.0Q, -2.0Q, -2.0Q },
  { -1.7Q, -2.0Q, -2.0Q, -1.0Q, -1.0Q },
  { -1.5Q, -2.0Q, -2.0Q, -1.0Q, -1.0Q },
  { -1.3Q, -1.0Q, -2.0Q, -1.0Q, -1.0Q },
  { +0.0Q, +0.0Q, +0.0Q, +0.0Q, +0.0Q },
  { +1.3Q, +1.0Q, +1.0Q, +2.0Q, +1.0Q },
  { +1.5Q, +2.0Q, +1.0Q, +2.0Q, +1.0Q },
  { +1.7Q, +2.0Q, +1.0Q, +2.0Q, +1.0Q },
  { +2.0Q, +2.0Q, +2.0Q, +2.0Q, +2.0Q }
};

int
main (void)
{
  size_t i;
  _Float128 v;

  for (i = 0; i < sizeof (a) / sizeof (a[0]); i++)
    {
      v = a[i].value;
      if (roundf128 (v) != a[i].exp_round)
	abort ();

      if (floorf128 (v) != a[i].exp_floor)
	abort ();

      if (ceilf128 (v) != a[i].exp_ceil)
	abort ();

      if (truncf128 (v) != a[i].exp_trunc)
	abort ();
    }

  return 0;
}
