/* { dg-do run } */
/* { dg-require-effective-target aarch64_sve2_hw } */
/* { dg-options "-O3 -mautovec-preference=sve-only" } */

#include "vect-sat-trunc-1.c"

/* Not a multiple of any SVE vector length, so that the loops run a
   predicated tail iteration.  */
#define N 257

#define INIT(IN, ARR)						\
  for (int i = 0; i < N; ++i)					\
    (ARR)[i] = (i & 1) ? (IN) (i - N / 2)			\
		       : (IN) (i * 0x9e3779b97f4a7c15ULL)

#define CHECK_UNSIGNED(NAME, OUT, IN, MAX)			\
  do {								\
    IN in[N];							\
    OUT out[N];							\
    INIT (IN, in);						\
    NAME (out, in, N);						\
    _Pragma ("GCC novector")					\
    for (int i = 0; i < N; ++i)					\
      {								\
	IN x = in[i];						\
	if (out[i] != (OUT) (x > (IN) MAX ? (IN) MAX : x))	\
	  __builtin_abort ();					\
      }								\
  } while (0)

#define CHECK_SIGNED(NAME, OUT, IN, MIN, MAX)			\
  do {								\
    IN in[N];							\
    OUT out[N];							\
    INIT (IN, in);						\
    NAME (out, in, N);						\
    _Pragma ("GCC novector")					\
    for (int i = 0; i < N; ++i)					\
      {								\
	IN x = in[i];						\
	OUT tmp = (OUT) x;					\
	OUT ref = ((IN) MIN <= x && x <= (IN) MAX		\
		   ? tmp : x < 0 ? (OUT) MIN : (OUT) MAX);	\
	if (out[i] != ref)					\
	  __builtin_abort ();					\
      }								\
  } while (0)

int
main (void)
{
  CHECK_UNSIGNED (u16_to_u8, uint8_t, uint16_t, 255);
  CHECK_UNSIGNED (u32_to_u16, uint16_t, uint32_t, 65535);
  CHECK_UNSIGNED (u64_to_u32, uint32_t, uint64_t, 4294967295ULL);
  CHECK_SIGNED (s16_to_s8, int8_t, int16_t, -128, 127);
  CHECK_SIGNED (s32_to_s16, int16_t, int32_t, -32768, 32767);
  CHECK_SIGNED (s64_to_s32, int32_t, int64_t, -2147483647 - 1, 2147483647);
  return 0;
}
