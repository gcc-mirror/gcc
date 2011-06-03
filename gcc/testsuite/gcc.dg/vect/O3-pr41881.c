/* { dg-do compile } */

#define TYPE int

TYPE fun1(TYPE *x, TYPE *y, unsigned int n)
{
  int i, j;
  TYPE dot = 0;

  for (i = 0; i < n; i++)
    dot += *(x++) * *(y++);

  return dot;
}

TYPE fun2(TYPE *x, TYPE *y, unsigned int n)
{
  int i, j;
  TYPE dot = 0;

  for (i = 0; i < n / 8; i++)
    for (j = 0; j < 8; j++)
      dot += *(x++) * *(y++);

  return dot;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" { target { vect_int_mult && {! vect_no_align } } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

