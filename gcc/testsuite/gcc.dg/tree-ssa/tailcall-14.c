/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-tailc-details" } */

/* PR tree-optimization/67797 */

void *my_func(void *s, int n)
{
  __builtin_memset(s, 0, n);
  return s;
}
void *my_func1(void *d, void *s, int n)
{
  __builtin_memcpy(d, s, n);
  return d;
}
void *my_func2(void *s, void *p1, int n)
{
  if (p1)
    __builtin_memcpy(s, p1, n);
  else
    __builtin_memset(s, 0, n);
  return s;
}

/* { dg-final { scan-tree-dump-times "Found tail call" 4 "tailc"} } */
