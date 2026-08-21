/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

#define CHECK(EXPR) \
  do \
    { \
      if (!(EXPR)) \
	__builtin_abort (); \
    } \
  while (0)

int
main (void)
{
  unsigned int b = svcntb ();
  unsigned int h = svcnth ();
  unsigned int w = svcntw ();
  unsigned int d = svcntd ();

  CHECK (b >= 16 && b <= 256);
  CHECK (h >= 8 && h <= 128);
  CHECK (w >= 4 && w <= 64);
  CHECK (d >= 2 && d <= 32);

  CHECK (b < 257);
  CHECK (h >= 8);
  CHECK (w <= 64);
  CHECK (d > 1);
  CHECK (b != 0);

  CHECK (!(b <= 15));
  CHECK (!(h < 8));
  CHECK (!(w > 64));
  CHECK (!(d == 0));

  CHECK ((svcntb () < 256) == (b < 256));
  CHECK ((svcntw () > 4) == (w > 4));

  CHECK (svcntb_pat (SV_ALL) == b);
  CHECK (svcntw_pat (SV_ALL) == w);
  CHECK (svcntb_pat (SV_ALL) < 257);
  CHECK (svcntw_pat (SV_ALL) <= 64);

  return 0;
}
