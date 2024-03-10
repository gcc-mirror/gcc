/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -moverride=sve_width=256 -fdump-tree-optimized" } */

/* This is a modified reduced version of cond_unary_5.c */

void __attribute__ ((noipa))
f0 (unsigned short *__restrict r,
   int *__restrict a,
   int *__restrict pred)
{
  for (int i = 0; i < 1024; ++i)
  {
    int p = pred[i]?-1:0;
    r[i] = p ;
  }
}

/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.h, p[0-7]+/z, #-1} 1 } } */
/* { dg-final { scan-assembler-not {\tmov\tz[0-9]+\.[hs], p[0-7]+/z, #1} } } */

/* { dg-final { scan-tree-dump-not "VIEW_CONVERT_EXPR " "optimized" } } */
/* { dg-final { scan-tree-dump-not " = -" "optimized" } } */
/* { dg-final { scan-tree-dump-not " = \\\(vector" "optimized" } } */
