/* { dg-do run { target bitint } } */
/* { dg-additional-options "-std=c23 -O0" } */

/* Test _BitInt bitwise operations for PowerPC.
   This test verifies that bitwise operations work correctly
   for various _BitInt sizes. */

int main(void)
{
  /* Test 32-bit bitwise operations */
  {
    _BitInt(32) a = 0xFF00FF00;
    _BitInt(32) b = 0x00FF00FF;

    _BitInt(32) and_result = a & b;
    _BitInt(32) or_result = a | b;
    _BitInt(32) xor_result = a ^ b;
    _BitInt(32) not_a = ~a;

    if (and_result != 0x00000000)
      __builtin_abort();
    if (or_result != 0xFFFFFFFF)
      __builtin_abort();
    if (xor_result != 0xFFFFFFFF)
      __builtin_abort();
    if (not_a != 0x00FF00FF)
      __builtin_abort();
  }

  /* Test shift operations */
  {
    _BitInt(32) a = 1;
    _BitInt(32) left = a << 10;
    _BitInt(32) b = 1024;
    _BitInt(32) right = b >> 5;

    if (left != 1024)
      __builtin_abort();
    if (right != 32)
      __builtin_abort();
  }

  /* Test 64-bit bitwise operations */
  {
    _BitInt(64) a = 0xFFFFFFFF00000000LL;
    _BitInt(64) b = 0x00000000FFFFFFFFLL;

    _BitInt(64) and_result = a & b;
    _BitInt(64) or_result = a | b;

    if (and_result != 0)
      __builtin_abort();
    if (or_result != 0xFFFFFFFFFFFFFFFFLL)
      __builtin_abort();
  }

  /* Test 128-bit bitwise operations */
  {
    _BitInt(128) a = 1;
    _BitInt(128) shifted = a << 100;
    _BitInt(128) back = shifted >> 100;

    if (back != 1)
      __builtin_abort();
  }

  /* Test with small _BitInt */
  {
    unsigned _BitInt(8) a = 0xAA;
    unsigned _BitInt(8) b = 0x55;
    unsigned _BitInt(8) xor_result = a ^ b;

    if (xor_result != (unsigned _BitInt(8)) 0xFF)
      __builtin_abort();
  }

  return 0;
}

/* Test passes if all bitwise operations work correctly */
