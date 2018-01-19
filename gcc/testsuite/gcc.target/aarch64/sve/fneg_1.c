/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O3 --save-temps" } */

#define DO_OPS(TYPE)					\
void vneg_##TYPE (TYPE *dst, TYPE *src, int count)	\
{							\
  for (int i = 0; i < count; ++i)			\
    dst[i] = -src[i];					\
}

DO_OPS (_Float16)
DO_OPS (float)
DO_OPS (double)

/* { dg-final { scan-assembler-times {\tfneg\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfneg\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfneg\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d\n} 1 } } */
