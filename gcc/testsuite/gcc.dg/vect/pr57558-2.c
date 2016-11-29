/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

void foo (int *a, int len)
{
  unsigned short i;

  for (i = 1; i < (len - 1); i++)
    a[i] = a[i+1];
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
