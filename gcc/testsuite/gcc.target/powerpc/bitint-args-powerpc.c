/* { dg-do run { target bitint } } */
/* { dg-additional-options "-std=c23 -O0" } */

/* Test _BitInt function arguments and return values for PowerPC.
   This test verifies that _BitInt values can be passed to and
   returned from functions correctly. */

_BitInt(32) add_bitint32(_BitInt(32) a, _BitInt(32) b)
{
  return a + b;
}

_BitInt(64) add_bitint64(_BitInt(64) a, _BitInt(64) b)
{
  return a + b;
}

_BitInt(128) add_bitint128(_BitInt(128) a, _BitInt(128) b)
{
  return a + b;
}

/* Wider than the by-reference threshold (16 bytes under ELFv2, 8 bytes
   otherwise), so passed and returned by reference on both ppc64 and
   ppc32.  */
_BitInt(129) add_bitint129(_BitInt(129) a, _BitInt(129) b)
{
  return a + b;
}

_BitInt(129) echo_bitint129(_BitInt(129) a)
{
  return a;
}

unsigned _BitInt(256) xor_bitint256(unsigned _BitInt(256) a,
				    unsigned _BitInt(256) b)
{
  return a ^ b;
}

_BitInt(200) identity_bitint200(_BitInt(200) x)
{
  return x;
}

_BitInt(200) add_large_bitint200(_BitInt(200) a, _BitInt(200) b)
{
  return a + b;
}

int main(void)
{
  /* Test 32-bit _BitInt argument passing */
  _BitInt(32) result32 = add_bitint32(100, 200);
  if (result32 != 300)
    __builtin_abort();

  /* Test 64-bit _BitInt argument passing */
  _BitInt(64) result64 = add_bitint64(1000000000LL, 2000000000LL);
  if (result64 != 3000000000LL)
    __builtin_abort();

  /* Test _BitInt(128) on both ppc32 and ppc64.  */
  {
    _BitInt(128) a = ((_BitInt(128)) 1 << 100) + 5;
    _BitInt(128) b = ((_BitInt(128)) 1 << 65) + 7;
    _BitInt(128) sum = add_bitint128(a, b);
    if (sum != (((_BitInt(128)) 1 << 100) + ((_BitInt(128)) 1 << 65) + 12))
      __builtin_abort();
  }

  /* Exercise large by-reference argument and return passing.  */
  {
    _BitInt(200) x = ((_BitInt(200)) 1 << 150) + ((_BitInt(200)) 1 << 99) + 11;
    _BitInt(200) y = ((_BitInt(200)) 1 << 149) + ((_BitInt(200)) 1 << 80) + 13;
    _BitInt(200) z = add_large_bitint200(x, y);
    if (identity_bitint200(z) != z)
      __builtin_abort();
    if (z != (((_BitInt(200)) 1 << 150) + ((_BitInt(200)) 1 << 149)
	      + ((_BitInt(200)) 1 << 99) + ((_BitInt(200)) 1 << 80) + 24))
      __builtin_abort();
  }

  {
    _BitInt(129) x = ((_BitInt(129)) 1 << 100) + 17;
    _BitInt(129) y = ((_BitInt(129)) 1 << 64) + 9;
    _BitInt(129) z = add_bitint129(x, y);
    if (z != (((_BitInt(129)) 1 << 100) + ((_BitInt(129)) 1 << 64) + 26))
      __builtin_abort();
    if (echo_bitint129(z) != z)
      __builtin_abort();
  }

  {
    unsigned _BitInt(256) x = ((unsigned _BitInt(256)) 0x12u << 200) | 0x34u;
    unsigned _BitInt(256) y = ((unsigned _BitInt(256)) 0x55u << 200) | 0x0Fu;
    if (xor_bitint256(x, y) != (((unsigned _BitInt(256)) 0x47u << 200) | 0x3Bu))
      __builtin_abort();
  }

  return 0;
}
