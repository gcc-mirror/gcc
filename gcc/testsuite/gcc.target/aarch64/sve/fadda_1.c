/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize --save-temps" } */

#define DO_OPS(TYPE)					\
TYPE fold_##TYPE (TYPE *src, int count)			\
{							\
  TYPE res = 0;						\
  for (int i = 0; i < count; ++i)			\
    res += src[i];					\
  return res;						\
}

DO_OPS (_Float16)
DO_OPS (float)
DO_OPS (double)

/* { dg-final { scan-assembler-times {\tfadda\th[0-9]+, p[0-7], h[0-9]+, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfadda\ts[0-9]+, p[0-7], s[0-9]+, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfadda\td[0-9]+, p[0-7], d[0-9]+, z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-not "sel" } } */
