/* { dg-do run { target bitint } } */
/* { dg-additional-options "-std=c23 -O0" } */

/* Test _BitInt type conversions for PowerPC.
   This test verifies that conversions between _BitInt types
   and standard integer types work correctly. */

int main(void)
{
  /* Test conversion from int to _BitInt */
  {
    int i = 42;
    _BitInt(32) bi = i;

    if (bi != 42)
      __builtin_abort();
  }

  /* Test conversion from _BitInt to int */
  {
    _BitInt(32) bi = 100;
    int i = (int)bi;

    if (i != 100)
      __builtin_abort();
  }

  /* Test conversion between different _BitInt sizes */
  {
    _BitInt(16) small = 1000;
    _BitInt(32) medium = small;
    _BitInt(64) large = medium;

    if (large != 1000)
      __builtin_abort();
  }

  /* Test narrowing conversion */
  {
    _BitInt(64) large = 0x12345678;
    _BitInt(32) medium = (_BitInt(32))large;

    if (medium != 0x12345678)
      __builtin_abort();
  }

  /* Test narrowing truncation where high bits are dropped.  */
  {
    _BitInt(64) wide = 0x12345678AABBCCDDLL;
    _BitInt(32) narrow = (_BitInt(32)) wide;

    if (narrow != (_BitInt(32)) 0xAABBCCDD)
      __builtin_abort();
  }

  /* Test truncation on narrowing to non-power-of-two widths.  */
  {
    _BitInt(32) large = 0x123;
    _BitInt(9) small = (_BitInt(9)) large;
    unsigned _BitInt(9) usmall = (unsigned _BitInt(9)) large;

    if (small != -221)
      __builtin_abort();
    if (usmall != 0x123u)
      __builtin_abort();
  }

  /* Test truncation from negative values.  */
  {
    _BitInt(16) neg = -1;
    unsigned _BitInt(9) u = (unsigned _BitInt(9)) neg;
    _BitInt(9) s = (_BitInt(9)) neg;

    if (u != 511)
      __builtin_abort();
    if (s != -1)
      __builtin_abort();
  }

  /* Test signed to unsigned conversion */
  {
    _BitInt(32) signed_val = -1;
    unsigned _BitInt(32) unsigned_val = (unsigned _BitInt(32))signed_val;

    if (unsigned_val != 0xFFFFFFFFU)
      __builtin_abort();
  }

  /* Test unsigned to signed conversion */
  {
    unsigned _BitInt(32) unsigned_val = 0x80000000U;
    _BitInt(32) signed_val = (_BitInt(32))unsigned_val;

    if (signed_val >= 0)
      __builtin_abort();
  }

  /* Test conversion from long long */
  {
    long long ll = 0x123456789ABCDEF0LL;
    _BitInt(64) bi = ll;

    if (bi != 0x123456789ABCDEF0LL)
      __builtin_abort();
  }

  /* Test conversion to long long */
  {
    _BitInt(64) bi = 0x0FEDCBA987654321LL;
    long long ll = (long long)bi;

    if (ll != 0x0FEDCBA987654321LL)
      __builtin_abort();
  }

  /* Test 128-bit conversions */
  {
    _BitInt(128) large = 1;
    large = large << 100;

    /* Convert to 64-bit (should truncate) */
    _BitInt(64) medium = (_BitInt(64))large;

    /* Lower 64 bits should be 0 */
    if (medium != 0)
      __builtin_abort();
  }

  /* Test conversion with sign extension */
  {
    _BitInt(8) small = -1;
    _BitInt(32) large = small;

    if (large != -1)
      __builtin_abort();
  }

  /* Test sign extension across a limb boundary.  */
  {
    _BitInt(7) small = -1;
    _BitInt(65) big = small;

    if (big != -1)
      __builtin_abort();
  }

  /* Test conversion with zero extension */
  {
    unsigned _BitInt(8) small = 255;
    unsigned _BitInt(32) large = small;

    if (large != 255)
      __builtin_abort();
  }

  /* Test conversion from floating-point types.  */
  {
    double d = 12345.0;
    _BitInt(32) bi = (_BitInt(32)) d;
    unsigned _BitInt(16) ubi = (unsigned _BitInt(16)) 255.0;
    double large_d = 1234567890.0;
    _BitInt(64) from_double = (_BitInt(64)) large_d;

    if (bi != 12345)
      __builtin_abort();
    if (ubi != 255)
      __builtin_abort();
    if (from_double != 1234567890wb)
      __builtin_abort();
  }

  /* Test floating-point conversion with truncation toward zero.  */
  {
    double d1 = 12.75;
    double d2 = -12.75;
    _BitInt(16) a = (_BitInt(16)) d1;
    _BitInt(16) b = (_BitInt(16)) d2;

    if (a != 12)
      __builtin_abort();
    if (b != -12)
      __builtin_abort();
  }

  /* Test conversion from char */
  {
    char c = 'A';
    _BitInt(8) bi = c;

    if (bi != 'A')
      __builtin_abort();
  }

  /* Test conversion to char */
  {
    _BitInt(8) bi = 66;
    char c = (char)bi;

    if (c != 'B')
      __builtin_abort();
  }

  /* Test conversion from _BitInt to floating-point.  */
  {
    _BitInt(32) bi = 2048;
    unsigned _BitInt(16) ubi = 511;
    double d = (double) bi;
    float f = (float) ubi;

    if (d != 2048.0)
      __builtin_abort();
    if (f != 511.0f)
      __builtin_abort();
  }

  return 0;
}

/* Test passes if all conversions work correctly */
