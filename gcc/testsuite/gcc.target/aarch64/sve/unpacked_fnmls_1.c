/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -moverride=sve_width=2048" } */

#include <stdint.h>

#define FMLA(SUFF)  __builtin_fma##SUFF (a[i], b[i], c[i])
#define FMLS(SUFF)  __builtin_fma##SUFF (a[i], -b[i], c[i])
#define FNMLA(SUFF) -FMLA (SUFF)
#define FNMLS(SUFF) -FMLS (SUFF)

#define TEST_FN(FN, TYPE0, TYPE1, COUNT)	\
  void						\
  f_##TYPE0##_##TYPE1 (TYPE1 *__restrict out,	\
		       TYPE0 *__restrict a,	\
		       TYPE0 *__restrict b,	\
		       TYPE0 *__restrict c,	\
		       TYPE0 *__restrict d)	\
  {						\
    for (unsigned int i = 0; i < COUNT; i++)	\
      if (FN > d[i])				\
	out[i] = 3;				\
  }

TEST_FN (FNMLS (f16), _Float16, uint64_t, 32)

TEST_FN (FNMLS (f16), _Float16, uint32_t, 64)

TEST_FN (FNMLS (f32), float, uint64_t, 32)

/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.s} 1 } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.d} 2 } } */

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d} 4 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.s} 4 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d} 4 } } */

/* { dg-final { scan-assembler-times {\t(fnmls|fnmsb)\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\t(fnmls|fnmsb)\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
