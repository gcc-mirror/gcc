/* { dg-do run } */
/* { dg-options "-O3 -march=armv8-a" } */

#include <stdint.h>

#define NELEMS 67

#define ROUND_NARROW(NAME, TO, FROM, N, DIV)				\
  __attribute__((noipa)) static void					\
  NAME (TO *__restrict d, const FROM *__restrict s, int n)		\
  {									\
    for (int i = 0; i < n; i++)						\
      d[i] = (TO) ((s[i] + (1 << (N - 1))) >> N);			\
  }									\
  __attribute__((noipa, optimize ("O0"))) static void			\
  NAME##_ref (TO *__restrict d, const FROM *__restrict s, int n)		\
  {									\
    for (int i = 0; i < n; i++)						\
      d[i] = (TO) ((s[i] + (1 << (N - 1))) >> N);			\
  }									\
  static void								\
  NAME##_check (void)							\
  {									\
    FROM s[NELEMS];							\
    TO d[NELEMS], r[NELEMS];						\
    for (int i = 0; i < NELEMS; i++)					\
      s[i] = (FROM) rnd () / (DIV);					\
    NAME (d, s, NELEMS);						\
    NAME##_ref (r, s, NELEMS);						\
    for (int i = 0; i < NELEMS; i++)					\
      if (d[i] != r[i])							\
	__builtin_abort ();						\
  }

static uint64_t seed = 0x123456789abcdefULL;

static uint64_t
rnd (void)
{
  seed = seed * 6364136223846793005ULL + 1442695040888963407ULL;
  return seed >> 11;
}

/* The signed 32-bit inputs are halved: signed overflow in the reference
   addition would be undefined.  The unsigned inputs are not halved, so the
   wrapping add is covered.  */

ROUND_NARROW (s16_s8, int8_t, int16_t, 6, 1)
ROUND_NARROW (u16_u8, uint8_t, uint16_t, 5, 1)
ROUND_NARROW (s32_s16, int16_t, int32_t, 8, 2)
ROUND_NARROW (u32_u16, uint16_t, uint32_t, 12, 1)

int
main (void)
{
  for (int rep = 0; rep < 64; rep++)
    {
      s16_s8_check ();
      u16_u8_check ();
      s32_s16_check ();
      u32_u16_check ();
    }
  return 0;
}
