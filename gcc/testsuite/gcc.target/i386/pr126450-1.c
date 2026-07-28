/* { dg-do run } */
/* { dg-options "-O2 -march=x86-64" } */

typedef int V [[gnu::vector_size (16)]];
long long a, b, c, d, e;
short f;
[[gnu::vector_size (8 * sizeof (int))]] int g;
V h;
_Bool i;

__attribute__((noipa, noinline, target("avx2")))
void
foo (V x)
{
  _Bool j = 0;
  short l = 0;
l1:
  b = l;
  x = h;
  l = c % 4;
  i = f = e % 6;
  e = x[j];
  g = g > g;
  if (d)
    goto l2;
l3:
  j = l;
  if (j)
    goto l1;
  a = l;
l2:
  if (e)
    goto l3;
}

int
main (void)
{
 if (__builtin_cpu_supports ("avx2"))
   foo (h);

  return 0;
}
