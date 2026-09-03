/* { dg-do run } */
/* { dg-require-effective-target aarch64_sve2_hw } */
/* { dg-options "-O3 -mautovec-preference=sve-only" } */

#include "vect-sat-trunc-2.c"

/* Not a multiple of any SVE vector length, so that the loops run a
   predicated tail iteration.  */
#define N 257

#define CHECK(NAME, OUT, IN, WIDE, MAX)					\
  do {									\
    IN in[N];								\
    OUT out[N];								\
    WIDE wide[N];							\
    for (int i = 0; i < N; ++i)						\
      in[i] = (i & 1) ? (IN) (i - N / 2)				\
		      : (IN) (i * 0x9e3779b97f4a7c15ULL);		\
    NAME (out, wide, in, N);						\
    _Pragma ("GCC novector")						\
    for (int i = 0; i < N; ++i)						\
      {									\
	IN x = in[i];							\
	if (out[i] != (OUT) (x > (IN) MAX ? (IN) MAX : x)		\
	    || wide[i] != (WIDE) x * 3)					\
	  __builtin_abort ();						\
      }									\
  } while (0)

int
main (void)
{
  CHECK (u16_to_u8_in_s, uint8_t, uint16_t, uint32_t, 255);
  CHECK (u16_to_u8_in_d, uint8_t, uint16_t, uint64_t, 255);
  CHECK (u32_to_u16_in_d, uint16_t, uint32_t, uint64_t, 65535);
  return 0;
}
