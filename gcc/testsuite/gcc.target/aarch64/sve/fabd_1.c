/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O3 --save-temps -fno-trapping-math" } */

#define N 16

typedef float *__restrict__ vnx4sf;
typedef double *__restrict__ vnx2df;
typedef _Float16 *__restrict__ vnx8hf_a;
typedef __fp16 *__restrict__ vnx8hf_b;

extern float fabsf (float);
extern double fabs (double);

#define FABD(type, abs, n)				\
	void fabd_##type (type res, type a, type b)	\
	{						\
	    int i;					\
	    for (i = 0; i < n; i++)			\
		res[i] = abs (a[i] - b[i]);		\
	}

#define TEST_SVE_F_MODES(FUNC)	\
  FUNC (vnx2df, fabs, N)	\
  FUNC (vnx4sf, fabsf, N)	\
  FUNC (vnx8hf_a, fabsf, N)	\
  FUNC (vnx8hf_b, fabsf, N)	\

TEST_SVE_F_MODES (FABD)

/* { dg-final { scan-assembler "fabd" } } */
/* { dg-final { scan-assembler-not "fsub" } } */
/* { dg-final { scan-assembler-not "fabs" } } */
/* { dg-final { scan-assembler-times {\tfabd\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfabd\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfabd\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 4 } } */
