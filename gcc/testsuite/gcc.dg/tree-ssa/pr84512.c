/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

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

/* Listed targets xfailed due to PR84958.  */
/* { dg-final { scan-tree-dump "return 285;" "optimized" { xfail { amdgcn*-*-* || vect_variable_length } } } } */
