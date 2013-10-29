/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mcpu=power8 -O2 -ftree-vectorize -fvect-cost-model=dynamic -fno-unroll-loops -fno-unroll-all-loops" } */

#include <altivec.h>

#ifndef SIZE
#define SIZE 1024
#endif

#ifndef ALIGN
#define ALIGN 32
#endif

#ifndef ATTR_ALIGN
#define ATTR_ALIGN __attribute__((__aligned__(ALIGN)))
#endif

#define DOIT(TYPE, PREFIX)						\
TYPE PREFIX ## _eqv_builtin (TYPE a, TYPE b)				\
{									\
  return vec_eqv (a, b);						\
}									\
									\
TYPE PREFIX ## _eqv_arith (TYPE a, TYPE b)				\
{									\
  return ~(a ^ b);							\
}									\
									\
TYPE PREFIX ## _nand_builtin (TYPE a, TYPE b)				\
{									\
  return vec_nand (a, b);						\
}									\
									\
TYPE PREFIX ## _nand_arith1 (TYPE a, TYPE b)				\
{									\
  return ~(a & b);							\
}									\
									\
TYPE PREFIX ## _nand_arith2 (TYPE a, TYPE b)				\
{									\
  return (~a) | (~b);							\
}									\
									\
TYPE PREFIX ## _orc_builtin (TYPE a, TYPE b)				\
{									\
  return vec_orc (a, b);						\
}									\
									\
TYPE PREFIX ## _orc_arith1 (TYPE a, TYPE b)				\
{									\
  return (~ a) | b;							\
}									\
									\
TYPE PREFIX ## _orc_arith2 (TYPE a, TYPE b)				\
{									\
  return a | (~ b);							\
}

#define DOIT_FLOAT(TYPE, PREFIX)					\
TYPE PREFIX ## _eqv_builtin (TYPE a, TYPE b)				\
{									\
  return vec_eqv (a, b);						\
}									\
									\
TYPE PREFIX ## _nand_builtin (TYPE a, TYPE b)				\
{									\
  return vec_nand (a, b);						\
}									\
									\
TYPE PREFIX ## _orc_builtin (TYPE a, TYPE b)				\
{									\
  return vec_orc (a, b);						\
}

typedef vector signed char		sign_char_vec;
typedef vector short			sign_short_vec;
typedef vector int			sign_int_vec;
typedef vector long long		sign_llong_vec;

typedef vector unsigned char		uns_char_vec;
typedef vector unsigned short		uns_short_vec;
typedef vector unsigned int		uns_int_vec;
typedef vector unsigned long long	uns_llong_vec;

typedef vector float			float_vec;
typedef vector double			double_vec;

DOIT(sign_char_vec,	sign_char)
DOIT(sign_short_vec,	sign_short)
DOIT(sign_int_vec,	sign_int)
DOIT(sign_llong_vec,	sign_llong)

DOIT(uns_char_vec,	uns_char)
DOIT(uns_short_vec,	uns_short)
DOIT(uns_int_vec,	uns_int)
DOIT(uns_llong_vec,	uns_llong)

DOIT_FLOAT(float_vec,	float)
DOIT_FLOAT(double_vec,	double)

/* { dg-final { scan-assembler-times "xxleqv"  18 } } */
/* { dg-final { scan-assembler-times "xxlnand" 26 } } */
/* { dg-final { scan-assembler-times "xxlorc"  26 } } */
