#include <limits.h>

extern void abort(void);
extern void exit(int);

#if __LONG_LONG_MAX__ == 9223372036854775807LL
#define BITS 64

static unsigned long long const zext[64] = {
  0x87654321fedcba90ULL,
  0x43b2a190ff6e5d48ULL,
  0x21d950c87fb72ea4ULL,
  0x10eca8643fdb9752ULL,
  0x87654321fedcba9ULL,
  0x43b2a190ff6e5d4ULL,
  0x21d950c87fb72eaULL,
  0x10eca8643fdb975ULL,
  0x87654321fedcbaULL,
  0x43b2a190ff6e5dULL,
  0x21d950c87fb72eULL,
  0x10eca8643fdb97ULL,
  0x87654321fedcbULL,
  0x43b2a190ff6e5ULL,
  0x21d950c87fb72ULL,
  0x10eca8643fdb9ULL,
  0x87654321fedcULL,
  0x43b2a190ff6eULL,
  0x21d950c87fb7ULL,
  0x10eca8643fdbULL,
  0x87654321fedULL,
  0x43b2a190ff6ULL,
  0x21d950c87fbULL,
  0x10eca8643fdULL,
  0x87654321feULL,
  0x43b2a190ffULL,
  0x21d950c87fULL,
  0x10eca8643fULL,
  0x87654321fULL,
  0x43b2a190fULL,
  0x21d950c87ULL,
  0x10eca8643ULL,
  0x87654321ULL,
  0x43b2a190ULL,
  0x21d950c8ULL,
  0x10eca864ULL,
  0x8765432ULL,
  0x43b2a19ULL,
  0x21d950cULL,
  0x10eca86ULL,
  0x876543ULL,
  0x43b2a1ULL,
  0x21d950ULL,
  0x10eca8ULL,
  0x87654ULL,
  0x43b2aULL,
  0x21d95ULL,
  0x10ecaULL,
  0x8765ULL,
  0x43b2ULL,
  0x21d9ULL,
  0x10ecULL,
  0x876ULL,
  0x43bULL,
  0x21dULL,
  0x10eULL,
  0x87ULL,
  0x43ULL,
  0x21ULL,
  0x10ULL,
  0x8ULL,
  0x4ULL,
  0x2ULL,
  0x1ULL
};

#elif __LONG_LONG_MAX__ == 2147483647LL
#define BITS 32

static unsigned long long const zext[32] = {
  0x87654321ULL,
  0x43b2a190ULL,
  0x21d950c8ULL,
  0x10eca864ULL,
  0x8765432ULL,
  0x43b2a19ULL,
  0x21d950cULL,
  0x10eca86ULL,
  0x876543ULL,
  0x43b2a1ULL,
  0x21d950ULL,
  0x10eca8ULL,
  0x87654ULL,
  0x43b2aULL,
  0x21d95ULL,
  0x10ecaULL,
  0x8765ULL,
  0x43b2ULL,
  0x21d9ULL,
  0x10ecULL,
  0x876ULL,
  0x43bULL,
  0x21dULL,
  0x10eULL,
  0x87ULL,
  0x43ULL,
  0x21ULL,
  0x10ULL,
  0x8ULL,
  0x4ULL,
  0x2ULL,
  0x1ULL,
};

#else
#error "Update the test case."
#endif

static unsigned long long
variable_shift(unsigned long long x, int i)
{
  return x >> i;
}

static unsigned long long
constant_shift(unsigned long long x, int i)
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
      unsigned long long y = variable_shift (zext[0], i);
      if (y != zext[i])
	abort ();
    }
  for (i = 0; i < BITS; ++i)
    {
      unsigned long long y = constant_shift (zext[0], i);
      if (y != zext[i])
	abort ();
    }

  exit (0);
}
