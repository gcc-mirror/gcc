#include <limits.h>

extern void abort(void);
extern void exit(int);

#if __LONG_LONG_MAX__ == 9223372036854775807LL
#define BITS 64

static long long const zext[64] = {
  0x7654321fedcba980LL,
  0x3b2a190ff6e5d4c0LL,
  0x1d950c87fb72ea60LL,
  0xeca8643fdb97530LL,
  0x7654321fedcba98LL,
  0x3b2a190ff6e5d4cLL,
  0x1d950c87fb72ea6LL,
  0xeca8643fdb9753LL,
  0x7654321fedcba9LL,
  0x3b2a190ff6e5d4LL,
  0x1d950c87fb72eaLL,
  0xeca8643fdb975LL,
  0x7654321fedcbaLL,
  0x3b2a190ff6e5dLL,
  0x1d950c87fb72eLL,
  0xeca8643fdb97LL,
  0x7654321fedcbLL,
  0x3b2a190ff6e5LL,
  0x1d950c87fb72LL,
  0xeca8643fdb9LL,
  0x7654321fedcLL,
  0x3b2a190ff6eLL,
  0x1d950c87fb7LL,
  0xeca8643fdbLL,
  0x7654321fedLL,
  0x3b2a190ff6LL,
  0x1d950c87fbLL,
  0xeca8643fdLL,
  0x7654321feLL,
  0x3b2a190ffLL,
  0x1d950c87fLL,
  0xeca8643fLL,
  0x7654321fLL,
  0x3b2a190fLL,
  0x1d950c87LL,
  0xeca8643LL,
  0x7654321LL,
  0x3b2a190LL,
  0x1d950c8LL,
  0xeca864LL,
  0x765432LL,
  0x3b2a19LL,
  0x1d950cLL,
  0xeca86LL,
  0x76543LL,
  0x3b2a1LL,
  0x1d950LL,
  0xeca8LL,
  0x7654LL,
  0x3b2aLL,
  0x1d95LL,
  0xecaLL,
  0x765LL,
  0x3b2LL,
  0x1d9LL,
  0xecLL,
  0x76LL,
  0x3bLL,
  0x1dLL,
  0xeLL,
  0x7LL,
  0x3LL,
  0x1LL,
  0LL
};

static long long const sext[64] = {
  0x8edcba9f76543210LL,
  0xc76e5d4fbb2a1908LL,
  0xe3b72ea7dd950c84LL,
  0xf1db9753eeca8642LL,
  0xf8edcba9f7654321LL,
  0xfc76e5d4fbb2a190LL,
  0xfe3b72ea7dd950c8LL,
  0xff1db9753eeca864LL,
  0xff8edcba9f765432LL,
  0xffc76e5d4fbb2a19LL,
  0xffe3b72ea7dd950cLL,
  0xfff1db9753eeca86LL,
  0xfff8edcba9f76543LL,
  0xfffc76e5d4fbb2a1LL,
  0xfffe3b72ea7dd950LL,
  0xffff1db9753eeca8LL,
  0xffff8edcba9f7654LL,
  0xffffc76e5d4fbb2aLL,
  0xffffe3b72ea7dd95LL,
  0xfffff1db9753eecaLL,
  0xfffff8edcba9f765LL,
  0xfffffc76e5d4fbb2LL,
  0xfffffe3b72ea7dd9LL,
  0xffffff1db9753eecLL,
  0xffffff8edcba9f76LL,
  0xffffffc76e5d4fbbLL,
  0xffffffe3b72ea7ddLL,
  0xfffffff1db9753eeLL,
  0xfffffff8edcba9f7LL,
  0xfffffffc76e5d4fbLL,
  0xfffffffe3b72ea7dLL,
  0xffffffff1db9753eLL,
  0xffffffff8edcba9fLL,
  0xffffffffc76e5d4fLL,
  0xffffffffe3b72ea7LL,
  0xfffffffff1db9753LL,
  0xfffffffff8edcba9LL,
  0xfffffffffc76e5d4LL,
  0xfffffffffe3b72eaLL,
  0xffffffffff1db975LL,
  0xffffffffff8edcbaLL,
  0xffffffffffc76e5dLL,
  0xffffffffffe3b72eLL,
  0xfffffffffff1db97LL,
  0xfffffffffff8edcbLL,
  0xfffffffffffc76e5LL,
  0xfffffffffffe3b72LL,
  0xffffffffffff1db9LL,
  0xffffffffffff8edcLL,
  0xffffffffffffc76eLL,
  0xffffffffffffe3b7LL,
  0xfffffffffffff1dbLL,
  0xfffffffffffff8edLL,
  0xfffffffffffffc76LL,
  0xfffffffffffffe3bLL,
  0xffffffffffffff1dLL,
  0xffffffffffffff8eLL,
  0xffffffffffffffc7LL,
  0xffffffffffffffe3LL,
  0xfffffffffffffff1LL,
  0xfffffffffffffff8LL,
  0xfffffffffffffffcLL,
  0xfffffffffffffffeLL,
  0xffffffffffffffffLL
};

#elif __LONG_LONG_MAX__ == 2147483647LL
#define BITS 32

static long long const zext[32] = {
  0x76543218LL,
  0x3b2a190cLL,
  0x1d950c86LL,
  0xeca8643LL,
  0x7654321LL,
  0x3b2a190LL,
  0x1d950c8LL,
  0xeca864LL,
  0x765432LL,
  0x3b2a19LL,
  0x1d950cLL,
  0xeca86LL,
  0x76543LL,
  0x3b2a1LL,
  0x1d950LL,
  0xeca8LL,
  0x7654LL,
  0x3b2aLL,
  0x1d95LL,
  0xecaLL,
  0x765LL,
  0x3b2LL,
  0x1d9LL,
  0xecLL,
  0x76LL,
  0x3bLL,
  0x1dLL,
  0xeLL,
  0x7LL,
  0x3LL,
  0x1LL,
  0LL
};

static long long const sext[64] = {
  0x87654321LL,
  0xc3b2a190LL,
  0xe1d950c8LL,
  0xf0eca864LL,
  0xf8765432LL,
  0xfc3b2a19LL,
  0xfe1d950cLL,
  0xff0eca86LL,
  0xff876543LL,
  0xffc3b2a1LL,
  0xffe1d950LL,
  0xfff0eca8LL,
  0xfff87654LL,
  0xfffc3b2aLL,
  0xfffe1d95LL,
  0xffff0ecaLL,
  0xffff8765LL,
  0xffffc3b2LL,
  0xffffe1d9LL,
  0xfffff0ecLL,
  0xfffff876LL,
  0xfffffc3bLL,
  0xfffffe1dLL,
  0xffffff0eLL,
  0xffffff87LL,
  0xffffffc3LL,
  0xffffffe1LL,
  0xfffffff0LL,
  0xfffffff8LL,
  0xfffffffcLL,
  0xfffffffeLL,
  0xffffffffLL
};

#else
#error "Update the test case."
#endif

static long long
variable_shift(long long x, int i)
{
  return x >> i;
}

static long long
constant_shift(long long x, int i)
{
  switch (i)
    {
    case 0: x = x >> 0; break;
    case 1: x = x >> 1; break;
    case 2: x = x >> 2; break;
    case 3: x = x >> 3; break;
    case 4: x = x >> 4; break;
    case 5: x = x >> 5; break;
    case 6: x = x >> 6; break;
    case 7: x = x >> 7; break;
    case 8: x = x >> 8; break;
    case 9: x = x >> 9; break;
    case 10: x = x >> 10; break;
    case 11: x = x >> 11; break;
    case 12: x = x >> 12; break;
    case 13: x = x >> 13; break;
    case 14: x = x >> 14; break;
    case 15: x = x >> 15; break;
    case 16: x = x >> 16; break;
    case 17: x = x >> 17; break;
    case 18: x = x >> 18; break;
    case 19: x = x >> 19; break;
    case 20: x = x >> 20; break;
    case 21: x = x >> 21; break;
    case 22: x = x >> 22; break;
    case 23: x = x >> 23; break;
    case 24: x = x >> 24; break;
    case 25: x = x >> 25; break;
    case 26: x = x >> 26; break;
    case 27: x = x >> 27; break;
    case 28: x = x >> 28; break;
    case 29: x = x >> 29; break;
    case 30: x = x >> 30; break;
    case 31: x = x >> 31; break;
#if BITS > 32
    case 32: x = x >> 32; break;
    case 33: x = x >> 33; break;
    case 34: x = x >> 34; break;
    case 35: x = x >> 35; break;
    case 36: x = x >> 36; break;
    case 37: x = x >> 37; break;
    case 38: x = x >> 38; break;
    case 39: x = x >> 39; break;
    case 40: x = x >> 40; break;
    case 41: x = x >> 41; break;
    case 42: x = x >> 42; break;
    case 43: x = x >> 43; break;
    case 44: x = x >> 44; break;
    case 45: x = x >> 45; break;
    case 46: x = x >> 46; break;
    case 47: x = x >> 47; break;
    case 48: x = x >> 48; break;
    case 49: x = x >> 49; break;
    case 50: x = x >> 50; break;
    case 51: x = x >> 51; break;
    case 52: x = x >> 52; break;
    case 53: x = x >> 53; break;
    case 54: x = x >> 54; break;
    case 55: x = x >> 55; break;
    case 56: x = x >> 56; break;
    case 57: x = x >> 57; break;
    case 58: x = x >> 58; break;
    case 59: x = x >> 59; break;
    case 60: x = x >> 60; break;
    case 61: x = x >> 61; break;
    case 62: x = x >> 62; break;
    case 63: x = x >> 63; break;
#endif

    default:
      abort ();
    }
  return x;
}

int
main()
{
  int i;

  for (i = 0; i < BITS; ++i)
    {
      long long y = variable_shift (zext[0], i);
      if (y != zext[i])
	abort ();
    }
  for (i = 0; i < BITS; ++i)
    {
      long long y = variable_shift (sext[0], i);
      if (y != sext[i])
	abort ();
    }
  for (i = 0; i < BITS; ++i)
    {
      long long y = constant_shift (zext[0], i);
      if (y != zext[i])
	abort ();
    }
  for (i = 0; i < BITS; ++i)
    {
      long long y = constant_shift (sext[0], i);
      if (y != sext[i])
	abort ();
    }

  exit (0);
}
