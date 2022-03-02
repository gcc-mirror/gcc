/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O3 -funsafe-math-optimizations" } */
/* Copied from gcc.c-torture/compile/20160205-1.c.  */

float a[32];
float fn1(int d) {
  float c = 4.0f;
  for (int b = 0; b < 32; b++)
    if (a[b] != 2.0f)
      c = 5.0f;
  return c;
}

/* { dg-final { scan-assembler-times {\t.word\t1073741824\n} 4 } } */ /* Constant 2.0f.  */
/* { dg-final { scan-assembler-times {\t.word\t1084227584\n} 4 } } */ /* Initial value for c (4.0).  */
/* { dg-final { scan-assembler-times {\t.word\t1082130432\n} 4 } } */ /* Possible value for c (5.0).  */
/* { dg-final { scan-assembler-not {\t.word\t1\n} } } */ /* 'true' mask.  */
/* { dg-final { scan-assembler-not {\t.word\t0\n} } } */ /* 'false' mask.  */
