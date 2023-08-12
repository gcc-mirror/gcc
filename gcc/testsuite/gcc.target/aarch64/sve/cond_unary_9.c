/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -moverride=sve_width=256 -fdump-tree-optimized" } */

/* This is a reduced version of cond_unary_5.c */

void __attribute__ ((noipa))
f (short *__restrict r,
   short *__restrict a,
   short *__restrict pred)
{
  for (int i = 0; i < 1024; ++i)
    r[i] = pred[i] != 0 ? ~(a[i]) : a[i];
}

/* { dg-final { scan-assembler-times {\tnot\tz[0-9]+\.h, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-not {\teor\tz} } } */
/* { dg-final { scan-assembler-not {\tmov\tz[0-9]+\.h, p[0-7]/m, #-1} } } */

/* { dg-final { scan-tree-dump-times ".COND_NOT " 1 "optimized" } } */
