/* { dg-do compile } */
/* { dg-options "-O3 -mavx512bw -mtune=znver4 -fdump-tree-vect-optimized" } */

int test (signed char *data, int n)
{
  int sum = 0;
  for (int i = 0; i < n; ++i)
    sum += data[i];
  return sum;
}

/* { dg-final { scan-tree-dump "loop vectorized using 64 byte vectors" "vect" } } */
/* { dg-final { scan-tree-dump "loop vectorized using 32 byte vectors" "vect" } } */
/* { dg-final { scan-tree-dump "loop vectorized using 16 byte vectors" "vect" } } */
/* { dg-final { scan-tree-dump "loop vectorized using 8 byte vectors" "vect" { target { ! ia32 } } } } */
