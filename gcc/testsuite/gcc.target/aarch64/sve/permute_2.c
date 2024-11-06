/* { dg-options "-O -msve-vector-bits=256" } */
/* { dg-final { check-function-bodies "**" "" "" { target { le } } } } */

typedef __SVUint16_t vuint16 __attribute__((arm_sve_vector_bits(256)));
typedef __SVFloat16_t vfloat16 __attribute__((arm_sve_vector_bits(256)));
typedef __SVBfloat16_t vbfloat16 __attribute__((arm_sve_vector_bits(256)));

#define TESTS(TYPE)							\
  TYPE									\
  TYPE##_zip1_d (TYPE x, TYPE y)					\
  {									\
    return __builtin_shufflevector (x, y, 0, 1, 2, 3, 16, 17, 18, 19,	\
				    4, 5, 6, 7, 20, 21, 22, 23);	\
  }									\
									\
  TYPE									\
  TYPE##_zip2_d (TYPE x, TYPE y)					\
  {									\
    return __builtin_shufflevector (x, y, 8, 9, 10, 11, 24, 25, 26, 27,	\
				    12, 13, 14, 15, 28, 29, 30, 31);	\
  }									\
									\
  TYPE									\
  TYPE##_trn1_d (TYPE x, TYPE y)					\
  {									\
    return __builtin_shufflevector (x, y, 0, 1, 2, 3, 16, 17, 18, 19,	\
				    8, 9, 10, 11, 24, 25, 26, 27);	\
  }									\
									\
  TYPE									\
  TYPE##_trn2_d (TYPE x, TYPE y)					\
  {									\
    return __builtin_shufflevector (x, y, 4, 5, 6, 7, 20, 21, 22, 23,	\
				    12, 13, 14, 15, 28, 29, 30, 31);	\
  }									\
									\
  TYPE									\
  TYPE##_uzp1_d (TYPE x, TYPE y)					\
  {									\
    return __builtin_shufflevector (x, y, 0, 1, 2, 3, 8, 9, 10, 11,	\
				    16, 17, 18, 19, 24, 25, 26, 27);	\
  }									\
									\
  TYPE									\
  TYPE##_uzp2_d (TYPE x, TYPE y)					\
  {									\
    return __builtin_shufflevector (x, y, 4, 5, 6, 7, 12, 13, 14, 15,	\
				    20, 21, 22, 23, 28, 29, 30, 31);	\
  }									\
									\
  TYPE									\
  TYPE##_zip1_s (TYPE x, TYPE y)					\
  {									\
    return __builtin_shufflevector (x, y, 0, 1, 16, 17, 2, 3, 18, 19,	\
				    4, 5, 20, 21, 6, 7, 22, 23);	\
  }									\
									\
  TYPE									\
  TYPE##_zip2_s (TYPE x, TYPE y)					\
  {									\
    return __builtin_shufflevector (x, y, 8, 9, 24, 25, 10, 11, 26, 27,	\
				    12, 13, 28, 29, 14, 15, 30, 31);	\
  }									\
									\
  TYPE									\
  TYPE##_trn1_s (TYPE x, TYPE y)					\
  {									\
    return __builtin_shufflevector (x, y, 0, 1, 16, 17, 4, 5, 20, 21,	\
				    8, 9, 24, 25, 12, 13, 28, 29);	\
  }									\
									\
  TYPE									\
  TYPE##_trn2_s (TYPE x, TYPE y)					\
  {									\
    return __builtin_shufflevector (x, y, 2, 3, 18, 19, 6, 7, 22, 23,	\
				    10, 11, 26, 27, 14, 15, 30, 31);	\
  }									\
									\
  TYPE									\
  TYPE##_uzp1_s (TYPE x, TYPE y)					\
  {									\
    return __builtin_shufflevector (x, y, 0, 1, 4, 5, 8, 9, 12, 13,	\
				    16, 17, 20, 21, 24, 25, 28, 29);	\
  }									\
									\
  TYPE									\
  TYPE##_uzp2_s (TYPE x, TYPE y)					\
  {									\
    return __builtin_shufflevector (x, y, 2, 3, 6, 7, 10, 11, 14, 15,	\
				    18, 19, 22, 23, 26, 27, 30, 31);	\
  }

/*
** vuint16_zip1_d:
**	zip1	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vuint16_zip2_d:
**	zip2	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vuint16_trn1_d:
**	trn1	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vuint16_trn2_d:
**	trn2	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vuint16_uzp1_d:
**	uzp1	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vuint16_uzp2_d:
**	uzp2	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vuint16_zip1_s:
**	zip1	z0\.s, z0\.s, z1\.s
**	ret
*/
/*
** vuint16_zip2_s:
**	zip2	z0\.s, z0\.s, z1\.s
**	ret
*/
/*
** vuint16_trn1_s:
**	trn1	z0\.s, z0\.s, z1\.s
**	ret
*/
/*
** vuint16_trn2_s:
**	trn2	z0\.s, z0\.s, z1\.s
**	ret
*/
/*
** vuint16_uzp1_s:
**	uzp1	z0\.s, z0\.s, z1\.s
**	ret
*/
/*
** vuint16_uzp2_s:
**	uzp2	z0\.s, z0\.s, z1\.s
**	ret
*/
TESTS (vuint16)

/*
** vfloat16_zip1_d:
**	zip1	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vfloat16_zip2_d:
**	zip2	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vfloat16_trn1_d:
**	trn1	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vfloat16_trn2_d:
**	trn2	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vfloat16_uzp1_d:
**	uzp1	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vfloat16_uzp2_d:
**	uzp2	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vfloat16_zip1_s:
**	zip1	z0\.s, z0\.s, z1\.s
**	ret
*/
/*
** vfloat16_zip2_s:
**	zip2	z0\.s, z0\.s, z1\.s
**	ret
*/
/*
** vfloat16_trn1_s:
**	trn1	z0\.s, z0\.s, z1\.s
**	ret
*/
/*
** vfloat16_trn2_s:
**	trn2	z0\.s, z0\.s, z1\.s
**	ret
*/
/*
** vfloat16_uzp1_s:
**	uzp1	z0\.s, z0\.s, z1\.s
**	ret
*/
/*
** vfloat16_uzp2_s:
**	uzp2	z0\.s, z0\.s, z1\.s
**	ret
*/
TESTS (vfloat16)

/*
** vbfloat16_zip1_d:
**	zip1	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vbfloat16_zip2_d:
**	zip2	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vbfloat16_trn1_d:
**	trn1	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vbfloat16_trn2_d:
**	trn2	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vbfloat16_uzp1_d:
**	uzp1	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vbfloat16_uzp2_d:
**	uzp2	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vbfloat16_zip1_s:
**	zip1	z0\.s, z0\.s, z1\.s
**	ret
*/
/*
** vbfloat16_zip2_s:
**	zip2	z0\.s, z0\.s, z1\.s
**	ret
*/
/*
** vbfloat16_trn1_s:
**	trn1	z0\.s, z0\.s, z1\.s
**	ret
*/
/*
** vbfloat16_trn2_s:
**	trn2	z0\.s, z0\.s, z1\.s
**	ret
*/
/*
** vbfloat16_uzp1_s:
**	uzp1	z0\.s, z0\.s, z1\.s
**	ret
*/
/*
** vbfloat16_uzp2_s:
**	uzp2	z0\.s, z0\.s, z1\.s
**	ret
*/
TESTS (vbfloat16)
