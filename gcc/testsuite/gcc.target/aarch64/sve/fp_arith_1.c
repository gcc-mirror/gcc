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
  DO_IMMEDIATE_OPS (1, TYPE, OP, NAME ## 1);			\
  DO_IMMEDIATE_OPS (0.5, TYPE, OP, NAME ## pointfive);		\
  DO_IMMEDIATE_OPS (2, TYPE, OP, NAME ## 2);			\
  DO_IMMEDIATE_OPS (2.5, TYPE, OP, NAME ## twopoint5);		\
  DO_IMMEDIATE_OPS (-0.5, TYPE, OP, NAME ## minuspointfive);	\
  DO_IMMEDIATE_OPS (-1, TYPE, OP, NAME ## minus1);

DO_ARITH_OPS (_Float16, +, add)
DO_ARITH_OPS (float, +, add)
DO_ARITH_OPS (double, +, add)

DO_ARITH_OPS (_Float16, -, minus)
DO_ARITH_OPS (float, -, minus)
DO_ARITH_OPS (double, -, minus)

/* No specific count because it's valid to use fadd or fsub for the
   out-of-range constants.  */
/* { dg-final { scan-assembler {\tfadd\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} } } */
/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #1.0\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #0.5\n} 2 } } */
/* { dg-final { scan-assembler-not   {\tfadd\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #2} } } */
/* { dg-final { scan-assembler-not   {\tfadd\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #-} } } */

/* { dg-final { scan-assembler {\tfsub\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} } } */
/* { dg-final { scan-assembler-times {\tfsub\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #1.0\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfsub\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #0.5\n} 2 } } */
/* { dg-final { scan-assembler-not   {\tfsub\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #2} } } */
/* { dg-final { scan-assembler-not   {\tfsub\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #-} } } */

/* { dg-final { scan-assembler {\tfadd\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} } } */
/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #1.0\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #0.5\n} 2 } } */
/* { dg-final { scan-assembler-not   {\tfadd\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #2} } } */
/* { dg-final { scan-assembler-not   {\tfadd\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #-} } } */

/* { dg-final { scan-assembler {\tfsub\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} } } */
/* { dg-final { scan-assembler-times {\tfsub\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #1.0\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfsub\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #0.5\n} 2 } } */
/* { dg-final { scan-assembler-not   {\tfsub\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #2} } } */
/* { dg-final { scan-assembler-not   {\tfsub\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #-} } } */

/* { dg-final { scan-assembler {\tfadd\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} } } */
/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, #1.0\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, #0.5\n} 2 } } */
/* { dg-final { scan-assembler-not   {\tfadd\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, #2} } } */
/* { dg-final { scan-assembler-not   {\tfadd\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, #-} } } */

/* { dg-final { scan-assembler {\tfsub\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} } } */
/* { dg-final { scan-assembler-times {\tfsub\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, #1.0\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfsub\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, #0.5\n} 2 } } */
/* { dg-final { scan-assembler-not   {\tfsub\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, #2} } } */
/* { dg-final { scan-assembler-not   {\tfsub\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, #-} } } */
