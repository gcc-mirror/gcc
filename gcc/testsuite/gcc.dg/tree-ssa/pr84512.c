/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */
/* { dg-require-effective-target vect_int_mult } */

int foo()
{
  int a[10];
  for(int i = 0; i < 10; ++i)
    a[i] = i*i;
  int res = 0;
  for(int i = 0; i < 10; ++i)
    res += a[i];
  return res;
}

/* { dg-final { scan-tree-dump "return 285;" "optimized" } } */
