/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#define DEF_LOOP(TYPE, NAME, CONST)			\
  void							\
  test_##TYPE##_##NAME (TYPE *restrict x,		\
			TYPE *restrict pred, int n)	\
  {							\
    for (int i = 0; i < n; ++i)				\
      x[i] = pred[i] > 0 ? CONST : 0;			\
  }

#define TEST_TYPE(T, TYPE)			\
  T (TYPE, 2, 2.0)				\
  T (TYPE, 1p25, 1.25)				\
  T (TYPE, 32p25, 32.25)			\
  T (TYPE, m4, -4.0)				\
  T (TYPE, m2p5, -2.5)				\
  T (TYPE, m64p5, -64.5)

#define TEST_ALL(T)				\
  TEST_TYPE (T, _Float16)			\
  TEST_TYPE (T, float)				\
  TEST_TYPE (T, double)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.h, p[0-9]+/z, #16384\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.h, p[0-9]+/z, #15616\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.h, p[0-9]+/z, #-15360\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.h, p[0-9]+/z, #-16128\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsel\tz[0-9]+\.h, p[0-9]+, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */

/* { dg-final { scan-assembler {\tmovprfx\t(z[0-9]+\.s), (p[0-7])/z, \1\n\tfmov\t\1, \2/m, #2\.0(?:e[+]0)?\n} } } */
/* { dg-final { scan-assembler {\tmovprfx\t(z[0-9]+\.s), (p[0-7])/z, \1\n\tfmov\t\1, \2/m, #1\.25(?:e[+]0)?\n} } } */
/* { dg-final { scan-assembler {\tmovprfx\t(z[0-9]+\.s), (p[0-7])/z, \1\n\tfmov\t\1, \2/m, #-4\.0(?:e[+]0)?\n} } } */
/* { dg-final { scan-assembler {\tmovprfx\t(z[0-9]+\.s), (p[0-7])/z, \1\n\tfmov\t\1, \2/m, #-2\.5(?:e[+]0)?\n} } } */
/* { dg-final { scan-assembler-times {\tsel\tz[0-9]+\.s, p[0-9]+, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */

/* { dg-final { scan-assembler {\tmovprfx\t(z[0-9]+\.d), (p[0-7])/z, \1\n\tfmov\t\1, \2/m, #2\.0(?:e[+]0)?\n} } } */
/* { dg-final { scan-assembler {\tmovprfx\t(z[0-9]+\.d), (p[0-7])/z, \1\n\tfmov\t\1, \2/m, #1\.25(?:e[+]0)?\n} } } */
/* { dg-final { scan-assembler {\tmovprfx\t(z[0-9]+\.d), (p[0-7])/z, \1\n\tfmov\t\1, \2/m, #-4\.0(?:e[+]0)?\n} } } */
/* { dg-final { scan-assembler {\tmovprfx\t(z[0-9]+\.d), (p[0-7])/z, \1\n\tfmov\t\1, \2/m, #-2\.5(?:e[+]0)?\n} } } */
/* { dg-final { scan-assembler-times {\tsel\tz[0-9]+\.d, p[0-9]+, z[0-9]+\.d, z[0-9]+\.d\n} 2 } } */
