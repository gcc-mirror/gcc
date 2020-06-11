/* { dg-do run } */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target stdint_types } */
/* { dg-options "-Wall" } */

#include <stdint.h>

#define MAKE_FUN(suffix, type)						\
  type my_bswap##suffix(type x) {					\
    type result = 0;							\
    int shift;								\
    for (shift = 0; shift < 8 * sizeof (type); shift += 8)	\
      {									\
	result <<= 8;							\
	result |= (x >> shift) & 0xff;					\
      }									\
    return result;							\
  }									\

MAKE_FUN(128, __uint128_t);

extern void abort (void);

typedef union
{
  struct { uint64_t lo; uint64_t hi; } s;
  __uint128_t n;
} u;

#define NUMS128							\
  {								\
    { .s = { 0x0000000000000000ULL, 0x1122334455667788ULL } }, 	\
    { .s = { 0x1122334455667788ULL, 0xffffffffffffffffULL } },	\
    { .s = { 0xffffffffffffffffULL, 0x0000000000000000ULL } }	\
  }

u uint128_ts[] = NUMS128;

#define N(table) (sizeof (table) / sizeof (table[0]))

int
main (void)
{
  int i;

  for (i = 0; i < N(uint128_ts); i++)
    if (__builtin_bswap128 (uint128_ts[i].n) != my_bswap128 (uint128_ts[i].n))
      abort ();

  return 0;
}
