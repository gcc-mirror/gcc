/* { dg-do compile } */
/* { dg-options "-dA" } */

typedef __SIZE_TYPE__ size_t;

static inline size_t foo (int n)
{
  return (n + sizeof (int) * 8 - 1) / (sizeof (int) * 8);
}

void bar (int, int *);

void baz (int n)
{
  int a[foo (n)];
  bar (n, a);
}
