/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize --save-temps" } */

#define DO_OPS(TYPE, OP)			\
void						\
vsqrt_##TYPE (TYPE *dst, TYPE *src, int count)	\
{						\
  for (int i = 0; i < count; ++i)		\
    dst[i] = __builtin_##OP (src[i]);		\
}

DO_OPS (_Float16, fabsf)
DO_OPS (float, fabsf)
DO_OPS (double, fabs)

/* { dg-final { scan-assembler-times {\tfabs\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfabs\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfabs\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d\n} 1 } } */
