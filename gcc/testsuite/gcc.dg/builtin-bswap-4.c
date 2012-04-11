/* { dg-do run } */
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

MAKE_FUN(16, uint16_t);
MAKE_FUN(32, uint32_t);
MAKE_FUN(64, uint64_t);

extern void abort (void);

#define NUMS16					\
  {						\
    0x0000,					\
    0x1122,					\
    0xffff,					\
  }

#define NUMS32					\
  {						\
    0x00000000UL,				\
    0x11223344UL,				\
    0xffffffffUL,				\
  }

#define NUMS64					\
  {						\
    0x0000000000000000ULL,			\
    0x1122334455667788ULL,			\
    0xffffffffffffffffULL,			\
  }

uint16_t uint16_ts[] =
  NUMS16;

uint32_t uint32_ts[] =
  NUMS32;

uint64_t uint64_ts[] =
  NUMS64;

#define N(table) (sizeof (table) / sizeof (table[0]))

int
main (void)
{
  int i;

  for (i = 0; i < N(uint16_ts); i++)
    if (__builtin_bswap16 (uint16_ts[i]) != my_bswap16 (uint16_ts[i]))
      abort ();

  for (i = 0; i < N(uint32_ts); i++)
    if (__builtin_bswap32 (uint32_ts[i]) != my_bswap32 (uint32_ts[i]))
      abort ();

  for (i = 0; i < N(uint64_ts); i++)
    if (__builtin_bswap64 (uint64_ts[i]) != my_bswap64 (uint64_ts[i]))
      abort ();

  return 0;
}
