/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -moverride=sve_width=256 -fdump-tree-optimized" } */
/* PR tree-optimization/111002 */

/* We should be able to remove the neg. */

void __attribute__ ((noipa))
f (int *__restrict r,
   int *__restrict a,
   short *__restrict pred)
{
  for (int i = 0; i < 1024; ++i)
    r[i] = pred[i] != 0 ? -1 : 0;
}


/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.h, p[0-7]+/z, #-1} 1 } } */
/* { dg-final { scan-assembler-not {\tmov\tz[0-9]+\.[hs], p[0-7]+/z, #1} } } */

/* { dg-final { scan-tree-dump-not "VIEW_CONVERT_EXPR " "optimized" } } */
/* { dg-final { scan-tree-dump-not " = -" "optimized" } } */
/* { dg-final { scan-tree-dump-not " = \\\(vector" "optimized" } } */
