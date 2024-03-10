/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */

int x[4092];
int y[1024];

void foo (int s)
{
  int i, j;
  for (i = 0, j = 0; j < 1023; i += s, j++)
    y[j] += x[i];
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
