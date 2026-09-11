/* { dg-do run { target bitint } } */
/* { dg-additional-options "-std=c23 -O0" } */

/* Test _BitInt comparison operations for PowerPC.
   This test verifies that comparison operations work correctly
   for various _BitInt sizes and signedness. */

int main(void)
{
  /* Test equality comparisons */
  {
    _BitInt(32) a = 100;
    _BitInt(32) b = 100;
    _BitInt(32) c = 200;

    if (!(a == b))
      __builtin_abort();
    if (a == c)
      __builtin_abort();
    if (!(a != c))
      __builtin_abort();
    if (a != b)
      __builtin_abort();
  }

  /* Test less than comparisons */
  {
    _BitInt(32) a = 50;
    _BitInt(32) b = 100;

    if (!(a < b))
      __builtin_abort();
    if (b < a)
      __builtin_abort();
    if (a < a)
      __builtin_abort();
  }

  /* Test greater than comparisons */
  {
    _BitInt(32) a = 200;
    _BitInt(32) b = 100;

    if (!(a > b))
      __builtin_abort();
    if (b > a)
      __builtin_abort();
    if (a > a)
      __builtin_abort();
  }

  /* Test less than or equal comparisons */
  {
    _BitInt(32) a = 100;
    _BitInt(32) b = 100;
    _BitInt(32) c = 200;

    if (!(a <= b))
      __builtin_abort();
    if (!(a <= c))
      __builtin_abort();
    if (c <= a)
      __builtin_abort();
  }

  /* Test greater than or equal comparisons */
  {
    _BitInt(32) a = 100;
    _BitInt(32) b = 100;
    _BitInt(32) c = 50;

    if (!(a >= b))
      __builtin_abort();
    if (!(a >= c))
      __builtin_abort();
    if (c >= a)
      __builtin_abort();
  }

  /* Test signed comparisons with negative values */
  {
    _BitInt(32) negative = -100;
    _BitInt(32) positive = 100;
    _BitInt(32) zero = 0;

    if (!(negative < zero))
      __builtin_abort();
    if (!(negative < positive))
      __builtin_abort();
    if (!(zero < positive))
      __builtin_abort();
    if (!(positive > negative))
      __builtin_abort();
  }

  /* Test unsigned comparisons */
  {
    unsigned _BitInt(32) a = 100;
    unsigned _BitInt(32) b = 200;

    if (!(a < b))
      __builtin_abort();
    if (!(b > a))
      __builtin_abort();
  }

  /* Test that large unsigned values compare correctly */
  {
    unsigned _BitInt(32) large = 0xFFFFFFFFU;
    unsigned _BitInt(32) small = 1;

    if (!(large > small))
      __builtin_abort();
    if (!(small < large))
      __builtin_abort();
  }

  /* Test 64-bit comparisons */
  {
    _BitInt(64) a = 0x123456789ABCDEF0LL;
    _BitInt(64) b = 0x123456789ABCDEF0LL;
    _BitInt(64) c = 0x0FEDCBA987654321LL;

    if (!(a == b))
      __builtin_abort();
    if (!(a > c))
      __builtin_abort();
    if (!(c < a))
      __builtin_abort();
  }

  /* Test 128-bit comparisons.  */
  {
    _BitInt(128) a = ((_BitInt(128)) 1 << 100) + 7;
    _BitInt(128) b = ((_BitInt(128)) 1 << 100) + 7;
    _BitInt(128) c = ((_BitInt(128)) 1 << 100) + 3;
    _BitInt(128) d = ((_BitInt(128)) 1 << 99) + 99;

    if (!(a == b))
      __builtin_abort();
    if (!(a > c))
      __builtin_abort();
    if (!(d < a))
      __builtin_abort();
  }

  /* Test cross-limb signed comparisons.  */
  {
    _BitInt(96) a = ((_BitInt(96)) 1 << 70) + 9;
    _BitInt(96) b = ((_BitInt(96)) 1 << 70) + 3;
    _BitInt(96) c = -(((_BitInt(96)) 1 << 70) - 5);
    _BitInt(96) d = -(((_BitInt(96)) 1 << 70) - 9);

    if (!(a > b))
      __builtin_abort();
    if (!(b < a))
      __builtin_abort();
    if (!(c < d))
      __builtin_abort();
    if (!(d > c))
      __builtin_abort();
    if (!(c < 0))
      __builtin_abort();
    if (!(a > 0))
      __builtin_abort();
  }

  /* Test small _BitInt comparisons, including sub-word signed values.  */
  {
    _BitInt(8) a = 50;
    _BitInt(8) b = 60;
    _BitInt(8) neg = -64;
    _BitInt(8) minus_one = -1;

    if (!(a < b))
      __builtin_abort();
    if (!(b > a))
      __builtin_abort();
    if (!(neg < minus_one))
      __builtin_abort();
    if (!(minus_one < 0))
      __builtin_abort();
    if (!(neg < 0))
      __builtin_abort();
    if (!((_BitInt(8)) 63 > (_BitInt(8)) -1))
      __builtin_abort();
  }

  /* Test sign handling when the sign bit lives in a partial top limb.  */
  {
    _BitInt(65) neg = -1;
    _BitInt(65) pos = 1;

    if (!(neg < pos))
      __builtin_abort();
    if (!(neg < 0))
      __builtin_abort();
    if (!(pos > neg))
      __builtin_abort();
  }

  /* Test unsigned sub-word comparisons with the high bit set.  */
  {
    unsigned _BitInt(9) large = 511;
    unsigned _BitInt(9) small = 1;

    if (!(large > small))
      __builtin_abort();
    if (!(small < large))
      __builtin_abort();
  }

  return 0;
}
