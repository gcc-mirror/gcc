/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O" } */
/* { dg-options "-O -msve-vector-bits=256" { target aarch64_sve256_hw } } */

#include "vec_perm_const_single_1.c"

#define TEST_VEC_PERM(TYPE, EXPECTED_RESULT, VALUES1, VALUES2)		\
{									\
  TYPE expected_result = EXPECTED_RESULT;				\
  TYPE values1 = VALUES1;						\
  TYPE values2 = VALUES2;						\
  TYPE dest;								\
  dest = vec_perm_##TYPE (values1, values2);				\
  if (__builtin_memcmp (&dest, &expected_result, sizeof (TYPE)) != 0)	\
    __builtin_abort ();							\
}

int main (void)
{
  TEST_VEC_PERM (vnx2di,
		 ((vnx2di) { 4, 7, 6, 5 }),
		 ((vnx2di) { 4, 5, 6, 7 }),
		 ((vnx2di) { 12, 24, 36, 48 }));
  TEST_VEC_PERM (vnx4si,
		 ((vnx4si) { 6, 10, 4, 3, 5, 7, 7, 5 }),
		 ((vnx4si) { 3, 4, 5, 6, 7, 8, 9, 10 }),
		 ((vnx4si) { 33, 34, 35, 36, 37, 38, 39, 40 }));
  TEST_VEC_PERM (vnx8hi,
		 ((vnx8hi) { 11, 10, 8, 7, 14, 15, 16, 3,
			     4, 4, 11, 12, 6, 17, 18, 4 }),
		 ((vnx8hi) { 3, 4, 5, 6, 7, 8, 9, 10,
			     11, 12, 13, 14, 15, 16, 17, 18 }),
		 ((vnx8hi) { 33, 34, 35, 36, 37, 38, 39, 40,
			     41, 42, 43, 44, 45, 46, 47, 48 }));
  TEST_VEC_PERM (vnx16qi,
		 ((vnx16qi) { 5, 5, 7, 6, 4, 4, 7, 4,
			      6, 7, 6, 5, 4, 7, 6, 7,
			      4, 5, 7, 7, 4, 7, 4, 5,
			      6, 7, 6, 7, 6, 4, 6, 5 }),
		 ((vnx16qi) { 4, 5, 6, 7, 4, 5, 6, 7,
			      4, 5, 6, 7, 4, 5, 6, 7,
			      4, 5, 6, 7, 4, 5, 6, 7,
			      4, 5, 6, 7, 4, 5, 6, 7 }),
		 ((vnx16qi) { 12, 24, 36, 48, 12, 24, 36, 48,
			      12, 24, 36, 48, 12, 24, 36, 48,
			      12, 24, 36, 48, 12, 24, 36, 48,
			      12, 24, 36, 48, 12, 24, 36, 48 }));
  TEST_VEC_PERM (vnx2df,
		 ((vnx2df) { 7.5, 7.5, 5.5, 5.5 }),
		 ((vnx2df) { 4.5, 5.5, 6.5, 7.5 }),
		 ((vnx2df) { 12.5, 24.5, 36.5, 48.5 }));
  TEST_VEC_PERM (vnx4sf,
		 ((vnx4sf) { 7.5, 8.5, 9.5, 3.5, 5.5, 10.5, 7.5, 5.5 }),
		 ((vnx4sf) { 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5 }),
		 ((vnx4sf) { 33.5, 34.5, 35.5, 36.5,
			     37.5, 38.5, 39.5, 40.5 }));
  TEST_VEC_PERM (vnx8hf,
		 ((vnx8hf) { 11.0, 10.0, 8.0, 7.0, 14.0, 15.0, 16.0, 3.0,
			     4.0, 4.0, 11.0, 12.0, 6.0, 17.0, 18.0, 4.0 }),
		 ((vnx8hf) { 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0,
			     11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0 }),
		 ((vnx8hf) { 33.0, 34.0, 35.0, 36.0, 37.0, 38.0, 39.0, 40.0,
			     41.0, 42.0, 43.0, 44.0, 45.0, 46.0, 47.0, 48.0 }));
  return 0;
}
