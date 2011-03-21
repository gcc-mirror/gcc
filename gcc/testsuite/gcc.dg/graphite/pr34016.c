/* PR tree-optimization/34016 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-linear" } */

extern void bar (double *);

void foo (void)
{
  double gr[36];
  int i, j;
  for (i = 0; i <= 5; i++)
    {
      for (j = 0; j <= 5; j++)
        gr[i + j * 6] = 0.0;
      if (i <= 2)
        gr[i + i * 6] = 1.0;
    }
  bar (gr);
}
