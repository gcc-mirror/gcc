/* { dg-do compile } */
/* { dg-additional-options "-O3 -fdump-tree-unrolljam-optimized" } */

int a[1024];
int b[2048];
int c[2048];

void foo(int n)
{
  for (int i = 0; i < n; i++)
    {
      int index = c[i];

      for (int j = 0; j < 1024; ++j)
        a[j] += b[index + j];
    }
}

/* { dg-final { scan-tree-dump "optimized: applying unroll and jam" "unrolljam" } } */
/* { dg-final { scan-tree-dump-times "optimized: loop vectorized" 2 "vect" { target vect_int } } } */
