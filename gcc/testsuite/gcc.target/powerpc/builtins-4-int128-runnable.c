/* { dg-do run } */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-options "-mpower8-vector" } */

#include <inttypes.h>
#include <altivec.h> // vector

#ifdef DEBUG
#include <stdio.h>
#endif

void abort (void);

int main() {
  int i;
  __uint128_t data_u128[100];
  __int128_t data_128[100];

  vector __int128_t vec_128_expected1, vec_128_result1;
  vector __uint128_t vec_u128_expected1, vec_u128_result1;
  signed long long zero = (signed long long) 0;

  for (i = 0; i < 100; i++)
    {
      data_128[i] = i + 12800000;
      data_u128[i] = i + 12800001;
    }

  /* vec_xl() tests */

  vec_128_expected1 = (vector __int128_t){12800000};
  vec_128_result1 = vec_xl (zero, data_128);

  if (vec_128_expected1[0] != vec_128_result1[0])
    {
#ifdef DEBUG
	printf("Error: vec_xl(), vec_128_result1[0] = %lld %llu; ",
	       vec_128_result1[0] >> 64,
	       vec_128_result1[0] & (__int128_t)0xFFFFFFFFFFFFFFFF);
	printf("vec_128_expected1[0] = %lld %llu\n",
	       vec_128_expected1[0] >> 64,
	       vec_128_expected1[0] & (__int128_t)0xFFFFFFFFFFFFFFFF);
#else
	abort ();
#endif
    }

  vec_u128_result1 = vec_xl (zero, data_u128);
  vec_u128_expected1 = (vector __uint128_t){12800001};
  if (vec_u128_expected1[0] != vec_u128_result1[0])
    {
#ifdef DEBUG
	printf("Error: vec_xl(), vec_u128_result1[0] = %lld; ",
	       vec_u128_result1[0] >> 64,
	       vec_u128_result1[0] & (__int128_t)0xFFFFFFFFFFFFFFFF);
	printf("vec_u128_expected1[0] = %lld\n",
	       vec_u128_expected1[0] >> 64,
	       vec_u128_expected1[0] & (__int128_t)0xFFFFFFFFFFFFFFFF);
#else
	abort ();
#endif
    }

  /* vec_xl_be() tests */

  vec_128_result1 = vec_xl_be (zero, data_128);
#ifdef __BIG_ENDIAN__
  vec_128_expected1 = (vector __int128_t){ (__int128_t)12800000 };
#else
  vec_128_expected1 = (vector __int128_t){ (__int128_t)12800000 };
#endif

  if (vec_128_expected1[0] != vec_128_result1[0])
    {
#ifdef DEBUG
	printf("Error: vec_xl_be(), vec_128_result1[0] = %llu %llu;",
	       vec_128_result1[0] >> 64,
	       vec_128_result1[0] & 0xFFFFFFFFFFFFFFFF);
	printf(" vec_128_expected1[0] = %llu %llu\n",
	       vec_128_expected1[0] >> 64,
	       vec_128_expected1[0] & 0xFFFFFFFFFFFFFFFF);
#else
      abort ();
#endif
    }

#ifdef __BIG_ENDIAN__
  vec_u128_expected1 = (vector __uint128_t){ (__uint128_t)12800001 };
#else
  vec_u128_expected1 = (vector __uint128_t){ (__uint128_t)12800001 };
#endif

  vec_u128_result1 = vec_xl_be (zero, data_u128);

  if (vec_u128_expected1[0] != vec_u128_result1[0])
    {
#ifdef DEBUG
	printf("Error: vec_xl_be(), vec_u128_result1[0] = %llu %llu;",
	       vec_u128_result1[0] >> 64,
	       vec_u128_result1[0] & 0xFFFFFFFFFFFFFFFF);
	printf(" vec_u128_expected1[0] = %llu %llu\n",
	       vec_u128_expected1[0] >> 64,
	       vec_u128_expected1[0] & 0xFFFFFFFFFFFFFFFF);
#else
      abort ();
#endif
    }
}
