/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

float *x;
float parm;
float
test (int start, int end)
{
  int i;
  for (i = start; i < end; ++i)
    {
      float tem = x[i];
      x[i] = parm * tem;
    }
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
