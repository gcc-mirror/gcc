/* { dg-options "-O -msve-vector-bits=256" } */
/* { dg-final { check-function-bodies "**" "" "" { target { le } } } } */

typedef __SVInt8_t vint8 __attribute__((arm_sve_vector_bits(256)));

#define TESTS(TYPE)							\
  TYPE									\
  TYPE##_zip1_d (TYPE x, TYPE y)					\
  {									\
    return __builtin_shufflevector (x, y, 0, 1, 2, 3, 4, 5, 6, 7,	\
				    32, 33, 34, 35, 36, 37, 38, 39,	\
				    8, 9, 10, 11, 12, 13, 14, 15,	\
				    40, 41, 42, 43, 44, 45, 46, 47);	\
  }									\
									\
  TYPE									\
  TYPE##_zip2_s (TYPE x, TYPE y)					\
  {									\
    return __builtin_shufflevector (x, y, 16, 17, 18, 19, 48, 49, 50, 51, \
				    20, 21, 22, 23, 52, 53, 54, 55,	\
				    24, 25, 26, 27, 56, 57, 58, 59,	\
				    28, 29, 30, 31, 60, 61, 62, 63);	\
  }									\
									\
  TYPE									\
  TYPE##_trn1_h (TYPE x, TYPE y)					\
  {									\
    return __builtin_shufflevector (x, y, 0, 1, 32, 33, 4, 5, 36, 37,	\
				    8, 9, 40, 41, 12, 13, 44, 45,	\
				    16, 17, 48, 49, 20, 21, 52, 53,	\
				    24, 25, 56, 57, 28, 29, 60, 61);	\
  }									\
									\
  TYPE									\
  TYPE##_trn2_d (TYPE x, TYPE y)					\
  {									\
    return __builtin_shufflevector (x, y, 8, 9, 10, 11, 12, 13, 14, 15,	\
				    40, 41, 42, 43, 44, 45, 46, 47,	\
				    24, 25, 26, 27, 28, 29, 30, 31,	\
				    56, 57, 58, 59, 60, 61, 62, 63);	\
  }									\
									\
  TYPE									\
  TYPE##_uzp1_s (TYPE x, TYPE y)					\
  {									\
    return __builtin_shufflevector (x, y, 0, 1, 2, 3, 8, 9, 10, 11,	\
				    16, 17, 18, 19, 24, 25, 26, 27,	\
				    32, 33, 34, 35, 40, 41, 42, 43,	\
				    48, 49, 50, 51, 56, 57, 58, 59);	\
  }									\
									\
  TYPE									\
  TYPE##_uzp2_h (TYPE x, TYPE y)					\
  {									\
    return __builtin_shufflevector (x, y, 2, 3, 6, 7, 10, 11, 14, 15,	\
				    18, 19, 22, 23, 26, 27, 30, 31,	\
				    34, 35, 38, 39, 42, 43, 46, 47,	\
				    50, 51, 54, 55, 58, 59, 62, 63);	\
  }

/*
** vint8_zip1_d:
**	zip1	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vint8_zip2_s:
**	zip2	z0\.s, z0\.s, z1\.s
**	ret
*/
/*
** vint8_trn1_h:
**	trn1	z0\.h, z0\.h, z1\.h
**	ret
*/
/*
** vint8_trn2_d:
**	trn2	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vint8_uzp1_s:
**	uzp1	z0\.s, z0\.s, z1\.s
**	ret
*/
/*
** vint8_uzp2_h:
**	uzp2	z0\.h, z0\.h, z1\.h
**	ret
*/
TESTS (vint8)
