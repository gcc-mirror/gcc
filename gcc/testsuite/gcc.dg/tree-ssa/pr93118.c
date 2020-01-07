/* PR tree-optimization/93118 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not ">>" "optimized" } } */
/* { dg-final { scan-tree-dump-not "<<" "optimized" } } */

#if __SIZEOF_LONG_LONG__ == 8 && __SIZEOF_INT__ == 4 && __CHAR_BIT__ == 8
unsigned long long
foo (unsigned long long a)
{
  unsigned long long b = a >> 32;
  int c = b;
  unsigned long long d = c;
  return d << 32;
}

unsigned long long
bar (unsigned long long a)
{
  unsigned long long b = a >> 32;
  unsigned c = b;
  unsigned long long d = c;
  return d << 32;
}

unsigned long long
baz (long long a)
{
  long long b = a >> 32;
  unsigned long long c = b;
  return c << 32;
}

typedef unsigned V __attribute__((vector_size (2 * sizeof (int))));
typedef int W __attribute__((vector_size (2 * sizeof (int))));

void
quux (W *w, V *v)
{
  W a = (W) (*v >> 16);
  *w = a << 16;
}
#else
int i;
#endif
