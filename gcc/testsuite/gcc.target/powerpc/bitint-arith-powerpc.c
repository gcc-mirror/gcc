/* { dg-do run { target bitint } } */
/* { dg-additional-options "-std=c23 -O0" } */

/* Test _BitInt arithmetic operations for PowerPC.
   This test verifies that basic arithmetic operations work correctly
   for various _BitInt sizes. */

int main(void)
{
  /* Test 32-bit arithmetic */
  {
    _BitInt(32) a = 100;
    _BitInt(32) b = 200;
    _BitInt(32) sum = a + b;
    _BitInt(32) diff = b - a;
    _BitInt(32) prod = a * b;
    _BitInt(32) quot = b / a;

    if (sum != 300)
      __builtin_abort();
    if (diff != 100)
      __builtin_abort();
    if (prod != 20000)
      __builtin_abort();
    if (quot != 2)
      __builtin_abort();
  }

  /* Test 64-bit arithmetic */
  {
    _BitInt(64) a = 1000000000LL;
    _BitInt(64) b = 2000000000LL;
    _BitInt(64) sum = a + b;
    _BitInt(64) prod = a * 3;
    _BitInt(64) carry_test = 0xFFFFFFFFLL;

    if (sum != 3000000000LL)
      __builtin_abort();
    if (prod != 3000000000LL)
      __builtin_abort();
    carry_test = carry_test + 1;
    if (carry_test != 0x100000000LL)
      __builtin_abort();
  }

  /* Test 128-bit arithmetic */
  {
    _BitInt(128) a = 1;
    _BitInt(128) b = 1;

    /* Compute 2^100 */
    for (int i = 0; i < 100; i++)
      a = a * 2;

    /* Compute 2^50 */
    for (int i = 0; i < 50; i++)
      b = b * 2;

    /* 2^100 / 2^50 should equal 2^50 */
    _BitInt(128) quot = a / b;
    if (quot != b)
      __builtin_abort();
  }

  /* Test signed arithmetic, including negative subtraction/borrow.  */
  {
    _BitInt(32) a = -100;
    _BitInt(32) b = 50;
    _BitInt(32) sum = a + b;
    _BitInt(32) diff = a - b;
    _BitInt(32) prod = a * b;
    _BitInt(32) quot = a / b;
    _BitInt(32) rem = a % b;

    if (sum != -50)
      __builtin_abort();
    if (diff != -150)
      __builtin_abort();
    if (prod != -5000)
      __builtin_abort();
    if (quot != -2)
      __builtin_abort();
    if (rem != 0)
      __builtin_abort();
  }

  /* Test signed wraparound semantics for narrow _BitInt.  */
  {
    _BitInt(8) max = 127;
    _BitInt(8) min = -128;

    if ((_BitInt(8)) (max + 1) != min)
      __builtin_abort();
    if ((_BitInt(8)) (min - 1) != max)
      __builtin_abort();
  }

  /* Test unsigned arithmetic, including modulo wraparound.  */
  {
    unsigned _BitInt(8) a = 255uwb;
    unsigned _BitInt(8) b = 1uwb;
    unsigned _BitInt(32) c = 100;
    unsigned _BitInt(32) d = 200;
    unsigned _BitInt(5) x = 31uwb;

    if ((unsigned _BitInt(8)) (a + b) != 0)
      __builtin_abort();
    if ((unsigned _BitInt(8)) (b - 2uwb) != 255uwb)
      __builtin_abort();
    if (c + d != 300)
      __builtin_abort();
    x = x + 1;
    if (x != 0)
      __builtin_abort();
  }

  return 0;
}
