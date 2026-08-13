/* { dg-do run } */
/* { dg-require-effective-target aarch64_sve_hw } */
/* { dg-options "-O3 -march=armv8.2-a+sve -mautovec-preference=sve-only" } */
/* { dg-additional-options "-msve-vector-bits=scalable" } */
/* { dg-additional-options "-fvect-cost-model=unlimited" } */

typedef __UINT8_TYPE__ uint8_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __UINT32_TYPE__ uint32_t;
typedef __UINT64_TYPE__ uint64_t;

#define N 67

#define DEFINE_RECUR(TYPE, SUFFIX) \
  __attribute__((noipa)) \
  static void \
  recur1_##SUFFIX (TYPE *__restrict a, TYPE *__restrict b, \
		   const TYPE *__restrict init) \
  { \
    TYPE prev = init[0]; \
    for (int i = 0; i < N; ++i) \
      { \
	b[i] = a[i] - prev; \
	prev = a[i]; \
      } \
  } \
  \
  __attribute__((noipa)) \
  static void \
  recur2_##SUFFIX (TYPE *__restrict a, TYPE *__restrict b, \
		   const TYPE *__restrict init) \
  { \
    TYPE prev0 = init[0]; \
    TYPE prev1 = init[0]; \
    for (int i = 0; i + 1 < N; i += 2) \
      { \
	b[i] = a[i] - prev0; \
	prev0 = a[i]; \
	b[i + 1] = a[i + 1] - prev1; \
	prev1 = a[i + 1]; \
      } \
    b[N - 1] = 0; \
  }

DEFINE_RECUR (uint8_t, u8)
DEFINE_RECUR (uint16_t, u16)
DEFINE_RECUR (uint32_t, u32)
DEFINE_RECUR (uint64_t, u64)

#define CHECK_RECUR(TYPE, SUFFIX) \
  do \
    { \
      TYPE a[N]; \
      TYPE b[N]; \
      const TYPE init[2] = { (TYPE) 19, (TYPE) 43 }; \
      \
      for (int i = 0; i < N; ++i) \
	a[i] = (TYPE) ((unsigned int) i * (unsigned int) i \
			  + 5U * (unsigned int) i + 11U); \
      \
      for (int seed = 0; seed < 2; ++seed) \
	{ \
	  recur1_##SUFFIX (a, b, &init[seed]); \
	  if (b[0] != (TYPE) (a[0] - init[seed])) \
	    __builtin_abort (); \
	  _Pragma ("GCC novector") \
	  for (int i = 1; i < N; ++i) \
	    if (b[i] != (TYPE) (a[i] - a[i - 1])) \
	      __builtin_abort (); \
	  \
	  recur2_##SUFFIX (a, b, &init[seed]); \
	  if (b[0] != (TYPE) (a[0] - init[seed]) \
	      || b[1] != (TYPE) (a[1] - init[seed])) \
	    __builtin_abort (); \
	  _Pragma ("GCC novector") \
	  for (int i = 2; i < N - 1; ++i) \
	    if (b[i] != (TYPE) (a[i] - a[i - 2])) \
	      __builtin_abort (); \
	  if (b[N - 1] != 0) \
	    __builtin_abort (); \
	} \
    } \
  while (0)

int
main (void)
{
  CHECK_RECUR (uint8_t, u8);
  CHECK_RECUR (uint16_t, u16);
  CHECK_RECUR (uint32_t, u32);
  CHECK_RECUR (uint64_t, u64);

  return 0;
}
