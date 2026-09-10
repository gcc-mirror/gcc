/* { dg-do run } */
/* { dg-options "-O2 -march=armv8-a" } */

#include <stdint.h>

typedef int8_t v8qi __attribute__((vector_size (8)));
typedef int16_t v8hi __attribute__((vector_size (16)));
typedef uint8_t v8uqi __attribute__((vector_size (8)));
typedef uint16_t v8uhi __attribute__((vector_size (16)));
typedef int16_t v4hi __attribute__((vector_size (8)));
typedef int32_t v4si __attribute__((vector_size (16)));
typedef uint16_t v4uhi __attribute__((vector_size (8)));
typedef uint32_t v4usi __attribute__((vector_size (16)));

#define ROUND(NAME, NARROW, WIDE, ELT, WELT, LANES, N)			\
  __attribute__((noipa)) static NARROW					\
  NAME (NARROW x)							\
  {									\
    WIDE w = __builtin_convertvector (x, WIDE);				\
    return __builtin_convertvector ((w + (1 << (N - 1))) >> N, NARROW);	\
  }									\
  __attribute__((noipa)) static void					\
  NAME##_check (void)							\
  {									\
    NARROW x, r;							\
    for (int rep = 0; rep < 4096; rep++)				\
      {									\
	for (int i = 0; i < LANES; i++)					\
	  x[i] = (ELT) rnd ();						\
	r = NAME (x);							\
	for (int i = 0; i < LANES; i++)					\
	  {								\
	    WELT w = (WELT) x[i] + (1 << (N - 1));			\
	    if (r[i] != (ELT) (w >> N))					\
	      __builtin_abort ();					\
	  }								\
      }									\
  }

static uint64_t seed = 0x123456789abcdefULL;

static uint64_t
rnd (void)
{
  seed = seed * 6364136223846793005ULL + 1442695040888963407ULL;
  return seed >> 11;
}

ROUND (s8, v8qi, v8hi, int8_t, int16_t, 8, 3)
ROUND (u8, v8uqi, v8uhi, uint8_t, uint16_t, 8, 3)
ROUND (s16, v4hi, v4si, int16_t, int32_t, 4, 6)
ROUND (u16, v4uhi, v4usi, uint16_t, uint32_t, 4, 6)

int
main (void)
{
  s8_check ();
  u8_check ();
  s16_check ();
  u16_check ();
  return 0;
}
