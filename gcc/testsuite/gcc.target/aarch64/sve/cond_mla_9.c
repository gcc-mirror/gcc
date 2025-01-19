/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#pragma GCC target "+sve2+sve-b16b16"

#define DEF_LOOP(TYPE, NAME, OP)			\
  void __attribute__ ((noipa))				\
  test_##TYPE##_##NAME (TYPE *__restrict r,		\
			TYPE *__restrict a,		\
			TYPE *__restrict b, TYPE c,	\
			short *__restrict pred, int n)	\
  {							\
    for (int i = 0; i < n; ++i)				\
      r[i] = pred[i] != 1 ? a[i] OP b[i] * c : a[i];	\
  }

DEF_LOOP (__bf16, add, +)
DEF_LOOP (__bf16, sub, -)

/* { dg-final { scan-assembler-times {\tbfmla\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tbfmls\tz[0-9]+\.h, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-not {\tmov\tz[^,]*z} } } */
/* { dg-final { scan-assembler-not {\tmovprfx\t} } } */
/* { dg-final { scan-assembler-not {\tsel\t} } } */
