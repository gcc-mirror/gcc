/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */

float x[256];

void foo(void)
{
  int i;
  for (i=0; i<256; ++i)
    x[i] = x[i] * x[i];
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
