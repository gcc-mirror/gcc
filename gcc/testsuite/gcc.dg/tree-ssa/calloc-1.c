/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

extern int a;
extern int *b;
int n;
void* f(long *q)
{
  int *p = __builtin_malloc (n);
  ++*q;
  if (p)
  {
    ++*q;
    a = 2;
    __builtin_memset (p, 0, n);
    *b = 3;
  }
  return p;
}
void* g(void)
{
  float *p = __builtin_calloc (8, 4);
  return __builtin_memset (p, 0, 24); // not 32
}

/* { dg-final { scan-tree-dump-times "calloc" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not "malloc" "optimized" } } */
/* { dg-final { scan-tree-dump-not "memset" "optimized" } } */
