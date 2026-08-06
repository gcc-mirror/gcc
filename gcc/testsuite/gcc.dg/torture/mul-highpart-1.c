/* Check that a 64-bit high-part multiply gives the same answer whether or not
   it is vectorized.  */
/* { dg-do run } */
/* { dg-require-effective-target int128 } */
/* { dg-additional-options "-ftree-vectorize" } */

#include <stdint.h>

#define N 77

static int64_t sa[N], sb[N], sd[N];
static uint64_t ua[N], ub[N], ud[N];

void __attribute__ ((noipa))
mulh_s64 (int64_t *restrict dst, int64_t *restrict a, int64_t *restrict b,
	  int count)
{
  for (int i = 0; i < count; ++i)
    dst[i] = (int64_t) (((__int128) a[i] * b[i]) >> 64);
}

void __attribute__ ((noipa))
mulh_u64 (uint64_t *restrict dst, uint64_t *restrict a, uint64_t *restrict b,
	  int count)
{
  for (int i = 0; i < count; ++i)
    dst[i] = (uint64_t) (((unsigned __int128) a[i] * b[i]) >> 64);
}

int
main (void)
{
  uint64_t s = 0x243f6a8885a308d3ULL;
  for (int i = 0; i < N; ++i)
    {
      s ^= s << 13; s ^= s >> 7; s ^= s << 17;
      sa[i] = (int64_t) s;
      ua[i] = s;
      s ^= s << 13; s ^= s >> 7; s ^= s << 17;
      sb[i] = (int64_t) s;
      ub[i] = s;
    }
  /* Boundary values.  */
  sa[0] = INT64_MIN; sb[0] = INT64_MIN;
  sa[1] = INT64_MIN; sb[1] = -1;
  sa[2] = -1; sb[2] = -1;
  ua[0] = 0; ub[0] = ~(uint64_t) 0;
  ua[1] = ~(uint64_t) 0; ub[1] = ~(uint64_t) 0;
  ua[2] = (uint64_t) 1 << 63; ub[2] = (uint64_t) 1 << 63;

  mulh_s64 (sd, sa, sb, N);
  mulh_u64 (ud, ua, ub, N);

  for (int i = 0; i < N; ++i)
    {
      if (sd[i] != (int64_t) (((__int128) sa[i] * sb[i]) >> 64))
	__builtin_abort ();
      if (ud[i] != (uint64_t) (((unsigned __int128) ua[i] * ub[i]) >> 64))
	__builtin_abort ();
    }
  return 0;
}
