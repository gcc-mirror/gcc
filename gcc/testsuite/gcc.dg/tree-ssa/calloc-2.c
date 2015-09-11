/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int n, nn;
void* f()
{
  char *p = __builtin_calloc (n, 1);
  p[42] = '\n';
  __builtin_memset (p, 0, nn);
  return p;
}

void* g(int m1, int m2)
{
  char *p = __builtin_malloc (m2);
  while (--m1)
  {
    __builtin_memset (p, 0, m2);
    p[n] = 'b';
  }
  return p;
}

/* { dg-final { scan-tree-dump-times "malloc" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "calloc" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "memset" 2 "optimized" } } */
