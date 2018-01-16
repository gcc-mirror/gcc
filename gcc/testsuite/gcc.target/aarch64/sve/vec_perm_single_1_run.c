/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O" } */
/* { dg-options "-O -msve-vector-bits=256" { target aarch64_sve256_hw } } */

#include "vec_perm_single_1.c"
extern void abort (void);

#define TEST_VEC_PERM(TYPE, MASK_TYPE, EXPECTED_RESULT, VALUES, MASK)	\
{									\
  TYPE expected_result = EXPECTED_RESULT;				\
  TYPE values = VALUES;							\
  MASK_TYPE mask = MASK;						\
  TYPE dest;								\
  dest = vec_perm_##TYPE (values, mask);				\
  if (__builtin_memcmp (&dest, &expected_result, sizeof (TYPE)) != 0)	\
    __builtin_abort ();							\
}

int main (void)
{
  TEST_VEC_PERM (vnx2di, vnx2di,
		 ((vnx2di) { 5, 6, 7, 5 }),
		 ((vnx2di) { 4, 5, 6, 7 }),
		 ((vnx2di) { 1, 6, 3, 5 }));
  TEST_VEC_PERM (vnx4si, vnx4si,
		 ((vnx4si) { 4, 8, 10, 10, 9, 8, 7, 5 }),
		 ((vnx4si) { 3, 4, 5, 6, 7, 8, 9, 10 }),
		 ((vnx4si) { 9, 13, 15, 7, 6, 5, 4, 10 }));
  TEST_VEC_PERM (vnx8hi, vnx8hi,
		 ((vnx8hi) { 12, 16, 18, 10, 12, 13, 14, 4,
			     7, 18, 3, 5, 9, 8, 7, 13 }),
		 ((vnx8hi) { 3, 4, 5, 6, 7, 8, 9, 10,
			     11, 12, 13, 14, 15, 16, 17, 18 }),
		 ((vnx8hi) { 9, 13, 15, 7, 25, 26, 27, 17,
			     4, 31, 0, 18, 6, 5, 4, 10 }));
  TEST_VEC_PERM (vnx16qi, vnx16qi,
		 ((vnx16qi) { 5, 6, 7, 4, 5, 6, 4, 5,
			      6, 7, 4, 5, 6, 7, 4, 5,
			      5, 6, 7, 4, 5, 6, 4, 5,
			      6, 7, 4, 5, 6, 7, 4, 5 }),
		 ((vnx16qi) { 4, 5, 6, 7, 4, 5, 6, 7,
			      4, 5, 6, 7, 4, 5, 6, 7,
			      4, 5, 6, 7, 4, 5, 6, 7,
			      4, 5, 6, 7, 4, 5, 6, 7 }),
		 ((vnx16qi) { 5, 6, 7, 8, 9, 10, 28, 29,
			      30, 31, 32, 33, 54, 55, 56, 61,
			      5, 6, 7, 8, 9, 10, 28, 29,
			      30, 31, 32, 33, 54, 55, 56, 61 }));
  TEST_VEC_PERM (vnx2df, vnx2di,
		 ((vnx2df) { 5.1, 6.1, 7.1, 5.1 }),
		 ((vnx2df) { 4.1, 5.1, 6.1, 7.1 }),
		 ((vnx2di) { 1, 6, 3, 5 }));
  TEST_VEC_PERM (vnx4sf, vnx4si,
		 ((vnx4sf) { 4.2, 8.2, 10.2, 10.2, 9.2, 8.2, 7.2, 5.2 }),
		 ((vnx4sf) { 3.2, 4.2, 5.2, 6.2, 7.2, 8.2, 9.2, 10.2 }),
		 ((vnx4si) { 9, 13, 15, 7, 6, 5, 4, 10 }));
  TEST_VEC_PERM (vnx8hf, vnx8hi,
		 ((vnx8hf) { 12.0, 16.0, 18.0, 10.0, 12.0, 13.0, 14.0, 4.0,
			     7.0, 18.0, 3.0, 5.0, 9.0, 8.0, 7.0, 13.0 }),
		 ((vnx8hf) { 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0,
			     11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0 }),
		 ((vnx8hi) { 9, 13, 15, 7, 25, 26, 27, 17,
			     4, 31, 0, 18, 6, 5, 4, 10 }));
  return 0;
}
