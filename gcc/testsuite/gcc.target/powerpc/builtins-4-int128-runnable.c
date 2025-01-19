/* { dg-do run } */
/* { dg-require-effective-target int128 } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-options "-mvsx" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */

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

#ifdef __BIG_ENDIAN__
  vector unsigned char vuc = {0xC, 0xD, 0xE, 0xF, 0x8, 0x9, 0xA, 0xB,
                              0x1C, 0x1D, 0x1E, 0x1F, 0x18, 0x19, 0x1A, 0x1B};
#else
  vector unsigned char vuc = {0x4, 0x5, 0x6, 0x7, 0x0, 0x1, 0x2, 0x3, 
			      0x14, 0x15, 0x16, 0x17, 0x10, 0x11, 0x12, 0x13};
#endif
  
  vector __int128_t vec_128_arg1, vec_128_arg2;
  vector __uint128_t vec_u128_arg1, vec_u128_arg2;
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
	       (unsigned long long)(vec_128_result1[0] >> 64),
	       (unsigned long long)(vec_128_result1[0]
				    & (__int128_t)0xFFFFFFFFFFFFFFFF));
	printf("vec_128_expected1[0] = %lld %llu\n",
	       (unsigned long long)(vec_128_expected1[0] >> 64),
	       (unsigned long long)(vec_128_expected1[0]
				    & (__int128_t)0xFFFFFFFFFFFFFFFF));
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
	       (unsigned long long)(vec_u128_result1[0] >> 64),
	       (unsigned long long)(vec_u128_result1[0]
				    & (__int128_t)0xFFFFFFFFFFFFFFFF));
	printf("vec_u128_expected1[0] = %lld\n",
	       (unsigned long long)(vec_u128_expected1[0] >> 64),
	       (unsigned long long)(vec_u128_expected1[0]
				    & (__int128_t)0xFFFFFFFFFFFFFFFF));
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
	       (unsigned long long)(vec_128_result1[0] >> 64),
	       (unsigned long long)(vec_128_result1[0] & 0xFFFFFFFFFFFFFFFF));
	printf(" vec_128_expected1[0] = %llu %llu\n",
	       (unsigned long long)(vec_128_expected1[0] >> 64),
	       (unsigned long long)(vec_128_expected1[0]
				    & 0xFFFFFFFFFFFFFFFF));
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
	       (unsigned long long)(vec_u128_result1[0] >> 64),
	       (unsigned long long)(vec_u128_result1[0] & 0xFFFFFFFFFFFFFFFF));
	printf(" vec_u128_expected1[0] = %llu %llu\n",
	       (unsigned long long)(vec_u128_expected1[0] >> 64),
	       (unsigned long long)(vec_u128_expected1[0]
				    & 0xFFFFFFFFFFFFFFFF));
#else
      abort ();
#endif
    }

  /* vec_perm() tests */
  vec_128_arg1 = (vector __int128_t){ (__uint128_t)0x1122334455667788ULL };
  vec_128_arg2 = (vector __int128_t){ (__uint128_t)0xAAABBBCCCDDDEEEF };

#ifdef __BIG_ENDIAN__
  vec_128_expected1[0] = 0x5566778811223344ULL;
  vec_128_expected1[0] = (vec_128_expected1[0] << 64) |
    0xcdddeeefaaabbbccULL;
#else
  vec_128_expected1[0] = 0xcdddeeefaaabbbccULL;
  vec_128_expected1[0] = (vec_128_expected1[0] << 64) |
    0x5566778811223344ULL;
#endif

  vec_128_result1 = vec_perm (vec_128_arg1, vec_128_arg2, vuc);

  if (vec_128_expected1[0] != vec_128_result1[0])
    {
#ifdef DEBUG
	printf("Error: vec_perm(), vec_128_result1[0] = %llu %llu;",
	       (unsigned long long)(vec_128_result1[0] >> 64),
	       (unsigned long long)(vec_128_result1[0] & 0xFFFFFFFFFFFFFFFF));
	printf(" vec_128_expected1[0] = %llu %llu\n",
	       (unsigned long long)(vec_128_expected1[0] >> 64),
	       (unsigned long long)(vec_128_expected1[0]
				    & 0xFFFFFFFFFFFFFFFF));
#else
      abort ();
#endif
    }
  vec_u128_arg1 = (vector __uint128_t){ (__uint128_t)0x1122334455667788ULL };
  vec_u128_arg2 = (vector __uint128_t){ (__uint128_t)0xAAABBBCCCDDDEEEF };

#ifdef __BIG_ENDIAN__
  vec_u128_expected1[0] = 0x5566778811223344ULL;
  vec_u128_expected1[0] = (vec_u128_expected1[0] << 64) |
    0xcdddeeefaaabbbccULL;
#else
  vec_u128_expected1[0] = 0xcdddeeefaaabbbccULL;
  vec_u128_expected1[0] = (vec_u128_expected1[0] << 64) |
    0x5566778811223344ULL;
#endif

  vec_u128_result1 = vec_perm (vec_u128_arg1, vec_u128_arg2, vuc);

  if (vec_u128_expected1[0] != vec_u128_result1[0])
    {
#ifdef DEBUG
	printf("Error: vec_perm(), vec_u128_result1[0] = %llu %llu;",
	       (unsigned long long)(vec_u128_result1[0] >> 64),
	       (unsigned long long)(vec_u128_result1[0] & 0xFFFFFFFFFFFFFFFF));
	printf(" vec_u128_expected1[0] = %llu %llu\n",
	       (unsigned long long)(vec_u128_expected1[0] >> 64),
	       (unsigned long long)(vec_u128_expected1[0]
				    & 0xFFFFFFFFFFFFFFFF));
#else
      abort ();
#endif
    }
}
