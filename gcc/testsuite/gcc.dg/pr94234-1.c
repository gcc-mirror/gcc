/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop1" } */ 

typedef __INTPTR_TYPE__ ssize_t;
typedef __PTRDIFF_TYPE__ ptrdiff_t;

ptrdiff_t foo (char *a, ssize_t n, ssize_t m)
{
  char *b1 = a + 8 * n;
  char *b2 = a + 8 * (n + 1);

  return (b1 + m) - (b2 + m);
}

/* { dg-final { scan-tree-dump-times "return -8;" 1 "forwprop1" } } */
