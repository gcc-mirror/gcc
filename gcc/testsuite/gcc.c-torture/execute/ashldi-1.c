#include <limits.h>

extern void abort(void);
extern void exit(int);

#if __LONG_LONG_MAX__ == 9223372036854775807LL
#define BITS 64

static unsigned long long const data[64] = {
  0x123456789abcdefULL,
  0x2468acf13579bdeULL,
  0x48d159e26af37bcULL,
  0x91a2b3c4d5e6f78ULL,
  0x123456789abcdef0ULL,
  0x2468acf13579bde0ULL,
  0x48d159e26af37bc0ULL,
  0x91a2b3c4d5e6f780ULL,
  0x23456789abcdef00ULL,
  0x468acf13579bde00ULL,
  0x8d159e26af37bc00ULL,
  0x1a2b3c4d5e6f7800ULL,
  0x3456789abcdef000ULL,
  0x68acf13579bde000ULL,
  0xd159e26af37bc000ULL,
  0xa2b3c4d5e6f78000ULL,
  0x456789abcdef0000ULL,
  0x8acf13579bde0000ULL,
  0x159e26af37bc0000ULL,
  0x2b3c4d5e6f780000ULL,
  0x56789abcdef00000ULL,
  0xacf13579bde00000ULL,
  0x59e26af37bc00000ULL,
  0xb3c4d5e6f7800000ULL,
  0x6789abcdef000000ULL,
  0xcf13579bde000000ULL,
  0x9e26af37bc000000ULL,
  0x3c4d5e6f78000000ULL,
  0x789abcdef0000000ULL,
  0xf13579bde0000000ULL,
  0xe26af37bc0000000ULL,
  0xc4d5e6f780000000ULL,
  0x89abcdef00000000ULL,
  0x13579bde00000000ULL,
  0x26af37bc00000000ULL,
  0x4d5e6f7800000000ULL,
  0x9abcdef000000000ULL,
  0x3579bde000000000ULL,
  0x6af37bc000000000ULL,
  0xd5e6f78000000000ULL,
  0xabcdef0000000000ULL,
  0x579bde0000000000ULL,
  0xaf37bc0000000000ULL,
  0x5e6f780000000000ULL,
  0xbcdef00000000000ULL,
  0x79bde00000000000ULL,
  0xf37bc00000000000ULL,
  0xe6f7800000000000ULL,
  0xcdef000000000000ULL,
  0x9bde000000000000ULL,
  0x37bc000000000000ULL,
  0x6f78000000000000ULL,
  0xdef0000000000000ULL,
  0xbde0000000000000ULL,
  0x7bc0000000000000ULL,
  0xf780000000000000ULL,
  0xef00000000000000ULL,
  0xde00000000000000ULL,
  0xbc00000000000000ULL,
  0x7800000000000000ULL,
  0xf000000000000000ULL,
  0xe000000000000000ULL,
  0xc000000000000000ULL,
  0x8000000000000000ULL
};

#elif __LONG_LONG_MAX__ == 2147483647LL
#define BITS 32

static unsigned long long const data[32] = {
  0x1234567fULL,
  0x2468acfeULL,
  0x48d159fcULL,
  0x91a2b3f8ULL,
  0x234567f0ULL,
  0x468acfe0ULL,
  0x8d159fc0ULL,
  0x1a2b3f80ULL,
  0x34567f00ULL,
  0x68acfe00ULL,
  0xd159fc00ULL,
  0xa2b3f800ULL,
  0x4567f000ULL,
  0x8acfe000ULL,
  0x159fc000ULL,
  0x2b3f8000ULL,
  0x567f0000ULL,
  0xacfe0000ULL,
  0x59fc0000ULL,
  0xb3f80000ULL,
  0x67f00000ULL,
  0xcfe00000ULL,
  0x9fc00000ULL,
  0x3f800000ULL,
  0x7f000000ULL,
  0xfe000000ULL,
  0xfc000000ULL,
  0xf8000000ULL,
  0xf0000000ULL,
  0xe0000000ULL,
  0xc0000000ULL,
  0x80000000ULL
};

#else
#error "Update the test case."
#endif

static unsigned long long
variable_shift(unsigned long long x, int i)
{
  return x << i;
}

static unsigned long long
constant_shift(unsigned long long x, int i)
{
  switch (i)
    {
    case 0: x = x << 0; break;
    case 1: x = x << 1; break;
    case 2: x = x << 2; break;
    case 3: x = x << 3; break;
    case 4: x = x << 4; break;
    case 5: x = x << 5; break;
    case 6: x = x << 6; break;
    case 7: x = x << 7; break;
    case 8: x = x << 8; break;
    case 9: x = x << 9; break;
    case 10: x = x << 10; break;
    case 11: x = x << 11; break;
    case 12: x = x << 12; break;
    case 13: x = x << 13; break;
    case 14: x = x << 14; break;
    case 15: x = x << 15; break;
    case 16: x = x << 16; break;
    case 17: x = x << 17; break;
    case 18: x = x << 18; break;
    case 19: x = x << 19; break;
    case 20: x = x << 20; break;
    case 21: x = x << 21; break;
    case 22: x = x << 22; break;
    case 23: x = x << 23; break;
    case 24: x = x << 24; break;
    case 25: x = x << 25; break;
    case 26: x = x << 26; break;
    case 27: x = x << 27; break;
    case 28: x = x << 28; break;
    case 29: x = x << 29; break;
    case 30: x = x << 30; break;
    case 31: x = x << 31; break;
#if BITS > 32
    case 32: x = x << 32; break;
    case 33: x = x << 33; break;
    case 34: x = x << 34; break;
    case 35: x = x << 35; break;
    case 36: x = x << 36; break;
    case 37: x = x << 37; break;
    case 38: x = x << 38; break;
    case 39: x = x << 39; break;
    case 40: x = x << 40; break;
    case 41: x = x << 41; break;
    case 42: x = x << 42; break;
    case 43: x = x << 43; break;
    case 44: x = x << 44; break;
    case 45: x = x << 45; break;
    case 46: x = x << 46; break;
    case 47: x = x << 47; break;
    case 48: x = x << 48; break;
    case 49: x = x << 49; break;
    case 50: x = x << 50; break;
    case 51: x = x << 51; break;
    case 52: x = x << 52; break;
    case 53: x = x << 53; break;
    case 54: x = x << 54; break;
    case 55: x = x << 55; break;
    case 56: x = x << 56; break;
    case 57: x = x << 57; break;
    case 58: x = x << 58; break;
    case 59: x = x << 59; break;
    case 60: x = x << 60; break;
    case 61: x = x << 61; break;
    case 62: x = x << 62; break;
    case 63: x = x << 63; break;
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
      unsigned long long y = variable_shift (data[0], i);
      if (y != data[i])
	abort ();
    }
  for (i = 0; i < BITS; ++i)
    {
      unsigned long long y = constant_shift (data[0], i);
      if (y != data[i])
	abort ();
    }

  exit (0);
}
