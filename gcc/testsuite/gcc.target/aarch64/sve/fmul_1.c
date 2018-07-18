/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O3 --save-temps" } */

#define DO_REGREG_OPS(TYPE, OP, NAME)				\
void varith_##TYPE##_##NAME (TYPE *dst, TYPE *src, int count)	\
{								\
  for (int i = 0; i < count; ++i)				\
    dst[i] = dst[i] OP src[i];					\
}

#define DO_IMMEDIATE_OPS(VALUE, TYPE, OP, NAME)		\
void varithimm_##NAME##_##TYPE (TYPE *dst, int count)	\
{							\
  for (int i = 0; i < count; ++i)			\
    dst[i] = dst[i] OP (TYPE) VALUE;			\
}

#define DO_ARITH_OPS(TYPE, OP, NAME)				\
  DO_REGREG_OPS (TYPE, OP, NAME);				\
  DO_IMMEDIATE_OPS (0.5, TYPE, OP, NAME ## 0point5);		\
  DO_IMMEDIATE_OPS (2, TYPE, OP, NAME ## 2);			\
  DO_IMMEDIATE_OPS (5, TYPE, OP, NAME ## 5);			\
  DO_IMMEDIATE_OPS (-0.5, TYPE, OP, NAME ## minus0point5);	\
  DO_IMMEDIATE_OPS (-2, TYPE, OP, NAME ## minus2);

DO_ARITH_OPS (_Float16, *, mul)
DO_ARITH_OPS (float, *, mul)
DO_ARITH_OPS (double, *, mul)

/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.h, z[0-9]+\.h, z[0-9]+\.h\n} 4 } } */
/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #0.5\n} 1 } } */
/* { dg-final { scan-assembler-not   {\tfmul\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #2} } } */
/* { dg-final { scan-assembler-not   {\tfmul\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #5} } } */
/* { dg-final { scan-assembler-not   {\tfmul\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #-} } } */

/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s\n} 4 } } */
/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #0.5\n} 1 } } */
/* { dg-final { scan-assembler-not   {\tfmul\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #2} } } */
/* { dg-final { scan-assembler-not   {\tfmul\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #5} } } */
/* { dg-final { scan-assembler-not   {\tfmul\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #-} } } */

/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 4 } } */
/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, #0.5\n} 1 } } */
/* { dg-final { scan-assembler-not   {\tfmul\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, #2} } } */
/* { dg-final { scan-assembler-not   {\tfmul\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, #5} } } */
/* { dg-final { scan-assembler-not   {\tfmul\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, #-} } } */
