/* { dg-options "-O -msve-vector-bits=256" } */
/* { dg-final { check-function-bodies "**" "" "" { target { le } } } } */

typedef __SVInt32_t vint32 __attribute__((arm_sve_vector_bits(256)));
typedef __SVFloat32_t vfloat32 __attribute__((arm_sve_vector_bits(256)));

#define TESTS(TYPE)							\
  TYPE									\
  TYPE##_zip1_d (TYPE x, TYPE y)					\
  {									\
    return __builtin_shufflevector (x, y, 0, 1, 8, 9, 2, 3, 10, 11);	\
  }									\
									\
  TYPE									\
  TYPE##_zip2_d (TYPE x, TYPE y)					\
  {									\
    return __builtin_shufflevector (x, y, 4, 5, 12, 13, 6, 7, 14, 15);	\
  }									\
									\
  TYPE									\
  TYPE##_trn1_d (TYPE x, TYPE y)					\
  {									\
    return __builtin_shufflevector (x, y, 0, 1, 8, 9, 4, 5, 12, 13);	\
  }									\
									\
  TYPE									\
  TYPE##_trn2_d (TYPE x, TYPE y)					\
  {									\
    return __builtin_shufflevector (x, y, 2, 3, 10, 11, 6, 7, 14, 15);	\
  }									\
									\
  TYPE									\
  TYPE##_uzp1_d (TYPE x, TYPE y)					\
  {									\
    return __builtin_shufflevector (x, y, 0, 1, 4, 5, 8, 9, 12, 13);	\
  }									\
									\
  TYPE									\
  TYPE##_uzp2_d (TYPE x, TYPE y)					\
  {									\
    return __builtin_shufflevector (x, y, 2, 3, 6, 7, 10, 11, 14, 15);	\
  }

/*
** vint32_zip1_d:
**	zip1	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vint32_zip2_d:
**	zip2	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vint32_trn1_d:
**	trn1	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vint32_trn2_d:
**	trn2	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vint32_uzp1_d:
**	uzp1	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vint32_uzp2_d:
**	uzp2	z0\.d, z0\.d, z1\.d
**	ret
*/
TESTS (vint32)

/*
** vfloat32_zip1_d:
**	zip1	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vfloat32_zip2_d:
**	zip2	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vfloat32_trn1_d:
**	trn1	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vfloat32_trn2_d:
**	trn2	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vfloat32_uzp1_d:
**	uzp1	z0\.d, z0\.d, z1\.d
**	ret
*/
/*
** vfloat32_uzp2_d:
**	uzp2	z0\.d, z0\.d, z1\.d
**	ret
*/
TESTS (vfloat32)
