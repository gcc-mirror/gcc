/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options { -std=gnu99 -Os -mcall-prologues -Wno-pedantic } } */

#include <stdfix.h>

#if __SIZEOF_LONG_DOUBLE__ == 8

typedef long double D;

volatile D d0 = 0;
volatile D dm15 = -1.5;
volatile D dp15 = +1.5;
volatile D dm05 = -0.5;
volatile D dp05 = +0.5;

void test0 (void)
{
  if (0hk != (short accum) d0)
    __builtin_exit (__LINE__);

  if (0uhk != (unsigned short accum) d0)
    __builtin_exit (__LINE__);

  if (0hr != (short fract) d0)
    __builtin_exit (__LINE__);

  if (0uhr != (unsigned short fract) d0)
    __builtin_exit (__LINE__);

  if (0k != (accum) d0)
    __builtin_exit (__LINE__);

  if (0uk != (unsigned accum) d0)
    __builtin_exit (__LINE__);

  if (0r != (fract) d0)
    __builtin_exit (__LINE__);

  if (0ur != (unsigned fract) d0)
    __builtin_exit (__LINE__);
}

void testp (void)
{
  if (0.5hr != (short fract) dp05)
    __builtin_exit (__LINE__);

  if (0.5uhr != (unsigned short fract) dp05)
    __builtin_exit (__LINE__);

  if (0.5r != (fract) dp05)
    __builtin_exit (__LINE__);

  if (0.5ur != (unsigned fract) dp05)
    __builtin_exit (__LINE__);

  if (1.5hk != (short accum) dp15)
    __builtin_exit (__LINE__);

  if (1.5uhk != (unsigned short accum) dp15)
    __builtin_exit (__LINE__);

  if (1.5k != (accum) dp15)
    __builtin_exit (__LINE__);

  if (1.5uk != (unsigned accum) dp15)
    __builtin_exit (__LINE__);
}

void testm (void)
{
  if (-0.5hr != (short fract) dm05)
    __builtin_exit (__LINE__);

  if (-0.5r != (fract) dm05)
    __builtin_exit (__LINE__);

  if (-1.5hk != (short accum) dm15)
    __builtin_exit (__LINE__);

  if (-1.5k != (accum) dm15)
    __builtin_exit (__LINE__);
}

int main (void)
{
  test0 ();
  testp ();
  testm ();

  return 0;
}
#else
int main (void)
{
  return 0;
}
#endif
