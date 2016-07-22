/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target powerpc_float128_hw_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-options "-O2 -mcpu=power9 -mfloat128 -mfloat128-hardware" } */

#ifndef NO_FLOAT
typedef _Complex float	float_complex;
extern float_complex cfloat1 (void);
extern float_complex cfloat2 (void);

#define FLOAT_ARG(NAME, OP)	ARG_OP(float, float_complex, NAME, OP)
#define FLOAT_PTR(NAME, OP)	PTR_OP(float, float_complex, NAME, OP)
#define FLOAT_CALL()		CALL_OP(float, float_complex, cfloat1, cfloat2)

#else
#define FLOAT_ARG(NAME, OP)
#define FLOAT_PTR(NAME, OP)
#define FLOAT_CALL()
#endif

#ifndef NO_DOUBLE
typedef _Complex double	double_complex;
extern double_complex cdouble1 (void);
extern double_complex cdouble2 (void);

#define DOUBLE_ARG(NAME, OP)	ARG_OP(double, double_complex, NAME, OP)
#define DOUBLE_PTR(NAME, OP)	PTR_OP(double, double_complex, NAME, OP)
#define DOUBLE_CALL()		CALL_OP(double, double_complex, cdouble1, cdouble2)

#else
#define DOUBLE_ARG(NAME, OP)
#define DOUBLE_PTR(NAME, OP)
#define DOUBLE_CALL()
#endif

#ifndef NO_FLOAT128
#ifdef __VSX__
typedef _Complex float __attribute__((mode(KC)))	float128_complex;
#else
typedef _Complex float __attribute__((mode(TC)))	float128_complex;
#endif

extern float128_complex cfloat128_1 (void);
extern float128_complex cfloat128_2 (void);

#define FLOAT128_ARG(NAME, OP)	ARG_OP(float128, float128_complex, NAME, OP)
#define FLOAT128_PTR(NAME, OP)	PTR_OP(float128, float128_complex, NAME, OP)
#define FLOAT128_CALL()		CALL_OP(float128, float128_complex, cfloat128_1, cfloat128_2)

#else
#define FLOAT128_ARG(NAME, OP)
#define FLOAT128_PTR(NAME, OP)
#define FLOAT128_CALL()
#endif

#ifndef NO_LDOUBLE
typedef _Complex long double ldouble_complex;
extern ldouble_complex cldouble1 (void);
extern ldouble_complex cldouble2 (void);

#define LDOUBLE_ARG(NAME, OP)	ARG_OP(ldouble, ldouble_complex, NAME, OP)
#define LDOUBLE_PTR(NAME, OP)	PTR_OP(ldouble, ldouble_complex, NAME, OP)
#define LDOUBLE_CALL()		CALL_OP(ldouble, ldouble_complex, cldouble1, cldouble2)

#else
#define LDOUBLE_ARG(NAME, OP)
#define LDOUBLE_PTR(NAME, OP)
#define LDOUBLE_CALL()
#endif


#define ARG_OP(SUFFIX, TYPE, NAME, OP)					\
TYPE arg_ ## NAME ## _ ## SUFFIX (TYPE a, TYPE b)			\
{									\
  return a OP b;							\
}

#define PTR_OP(SUFFIX, TYPE, NAME, OP)					\
void ptr_ ## NAME ## _ ## SUFFIX (TYPE *p, TYPE *a, TYPE *b)		\
{									\
  *p = *a OP *b;							\
}

#define CALL_OP(SUFFIX, TYPE, FUNC1, FUNC2)				\
TYPE call_ ## SUFFIX (void)						\
{									\
  TYPE value1 = FUNC1 ();						\
  TYPE value2 = FUNC2 ();						\
  return value1 + value2;						\
}

#ifndef NO_ARG
#ifndef NO_ADD
FLOAT_ARG    (add, +)
DOUBLE_ARG   (add, +)
FLOAT128_ARG (add, +)
LDOUBLE_ARG  (add, +)
#endif

#ifndef NO_SUB
FLOAT_ARG    (sub, -)
DOUBLE_ARG   (sub, -)
FLOAT128_ARG (sub, -)
LDOUBLE_ARG  (sub, -)
#endif

#ifndef NO_MUL
FLOAT_ARG    (mul, *)
DOUBLE_ARG   (mul, *)
FLOAT128_ARG (mul, *)
LDOUBLE_ARG  (mul, *)
#endif

#ifndef NO_DIV
FLOAT_ARG    (div, /)
DOUBLE_ARG   (div, /)
FLOAT128_ARG (div, /)
LDOUBLE_ARG  (div, /)
#endif
#endif

#ifndef NO_PTR
#ifndef NO_ADD
FLOAT_PTR    (add, +)
DOUBLE_PTR   (add, +)
FLOAT128_PTR (add, +)
LDOUBLE_PTR  (add, +)
#endif

#ifndef NO_SUB
FLOAT_PTR    (sub, -)
DOUBLE_PTR   (sub, -)
FLOAT128_PTR (sub, -)
LDOUBLE_PTR  (sub, -)
#endif

#ifndef NO_MUL
FLOAT_PTR    (mul, *)
DOUBLE_PTR   (mul, *)
FLOAT128_PTR (mul, *)
LDOUBLE_PTR  (mul, *)
#endif

#ifndef NO_DIV
FLOAT_PTR    (div, /)
DOUBLE_PTR   (div, /)
FLOAT128_PTR (div, /)
LDOUBLE_PTR  (div, /)
#endif
#endif

#ifndef NO_CALL
FLOAT_CALL    ()
DOUBLE_CALL   ()
FLOAT128_CALL ()
LDOUBLE_CALL  ()
#endif

/* { dg-final { scan-assembler "xsaddqp"  } } */
/* { dg-final { scan-assembler "xssubqp"  } } */
